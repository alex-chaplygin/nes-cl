#include <stdlib.h>
#include <SDL.h>
typedef unsigned char byte;
#define SCREEN_WIDTH 256	/**< размеры PPU */
#define SCREEN_HEIGHT 240

int window_width;	/**< размеры окна */
int window_height;
byte *video_buffer;		/**< видео буфер экрана*/
SDL_Window *window;		/**< окно SDL */
SDL_Renderer *renderer;		/**< устройство вывода */
SDL_Surface *screen;		/**< поверхность экрана без курсора мыши */
byte palette[768]; /**< палитра */
int r_mask = 1;
int g_mask = 1;
int b_mask = 1;

void set_palette();

/** 
 * Инициализация графического интерфейса
 */
void video_init(int scale)
{
  window_width = SCREEN_WIDTH * scale;
  window_height = SCREEN_HEIGHT * scale;
  SDL_Init(SDL_INIT_VIDEO | SDL_INIT_EVENTS);
  window = SDL_CreateWindow("NES", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, window_width, window_height, SDL_WINDOW_SHOWN);
  if (window == NULL) {
    printf("Cannot create window\n");
    exit(1);
  }
  renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
  if (renderer == NULL) {
    printf("Cannot create renderer\n");
    exit(1);
  }
  SDL_ShowCursor(SDL_DISABLE);
  //  SDL_SetColorKey(cursor_surface, SDL_TRUE, 0);
  screen = SDL_CreateRGBSurface(0, SCREEN_WIDTH, SCREEN_HEIGHT, 8, 0, 0, 0, 0);
  FILE *f = fopen("ntscpalette.pal", "rb");
  if (!f) {
    printf("No palette file\n");
    //    exit(1);
  }
  fread(palette, 192, 1, f);
  fclose(f);
  set_palette();
  SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, "1");
  SDL_LockSurface(screen);
  video_buffer = screen->pixels;
}

/// Обработка событий клавиатуры и мыши
int video_get_events()
{
  SDL_Event e;
  if (SDL_PollEvent(&e)) {
    if (e.type == SDL_QUIT)
      return 0;
    /*    else if (e.type == SDL_KEYDOWN)
      set_key(e.key.keysym.scancode, e.key.keysym.sym, e.key.keysym.mod);
    else if (e.type == SDL_KEYUP)
    release_key(e.key.keysym.scancode);*/
  }
  return 1;
}

/** 
 * Обновление графики.
 * Обработка событий клавиатуры, мыши.
 * Обновление таймера, задержка кадра для постоянного fps.
 * Отрисовка экранного буфера.
 */
void video_update(byte *buf)
{
  memcpy(video_buffer, buf, SCREEN_WIDTH * SCREEN_HEIGHT);
  SDL_UnlockSurface(screen);
  SDL_Texture* t = SDL_CreateTextureFromSurface(renderer, screen);
  SDL_RenderCopy(renderer, t, NULL, NULL);
  SDL_DestroyTexture(t);
  SDL_RenderPresent(renderer);
  SDL_LockSurface(screen);
  video_buffer = screen->pixels;
}

/** 
 * Завершение графики, закрытие окна
 */
void video_close()
{
  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);
  SDL_Quit();
}

/// Ожидание одного кадра
void video_sleep(long l)
{
    SDL_Delay(l);
}

long video_get_time()
{
  return SDL_GetTicks();
}

/** 
 * Установка новой палитры
 * 
 * @param palette данные палитры: 3 * 256 = 768 байт
 */
void set_palette()
{
  SDL_Color colors[256];
  byte *dst = palette;
  for(int i = 0; i < 256; i++) {
    colors[i].r = *dst++ * r_mask;
    colors[i].g = *dst++ * g_mask;
    colors[i].b = *dst++ * b_mask;
  }
  if (SDL_SetPaletteColors(screen->format->palette, colors, 0, 256)) {
    printf("palette error\n");
    exit(1);
  }
}

void set_palette_mask(int r, int g, int b)
{
  r_mask = r;
  g_mask = g;
  b_mask = b;
  set_palette();
}
