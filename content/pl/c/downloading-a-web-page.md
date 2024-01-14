---
title:                "C: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Dlaczego

Aby rozpocząć pisanie aplikacji internetowych w C, musimy nauczyć się pobierać strony internetowe. W tym blogu dowiesz się, jak to zrobić!

## Jak to zrobić

Pobieranie strony internetowej w języku C może wydawać się trudne na pierwszy rzut oka, ale jest to jedynie kilka kroków. Najpierw musimy zawrzeć bibliotekę `stdio.h` w celu korzystania z funkcji `fopen()` oraz `fclose()`. Następnie definiujemy zmienną `FILE`, która będzie używana do otwierania i zamykania plików. W końcu, używając funkcji `fopen()` wraz z adresem URL, możemy pobrać dane z witryny. Oto przykładowy kod:

```c
#include <stdio.h>

int main() {
  // deklaracja zmiennej FILE
  FILE *plik;
  
  // otwieramy plik ze stroną internetową
  plik = fopen("http://www.example.com", "r");
  
  if (plik == NULL) {
    printf("Nie można otworzyć pliku!");
    return 1;
  }
  
  // pobieramy dane ze strony i zapisujemy je w pliku
  int pobrane_dane;
  
  while ((pobrane_dane = fgetc(plik)) != EOF) {
    printf("%c", pobrane_dane);
  }
  
  // zamykamy plik
  fclose(plik);
  return 0;
}
```

Przykładowy output może wyglądać następująco:

```
<!doctype html>
<html>
<head>
  <title>Przykładowa strona internetowa</title>
</head>
<body>
  <h1>Witaj na przykładowej stronie internetowej!</h1>
  <p>Jest to tylko przykład, ale zobacz, jak łatwo można pobrać treść z dowolnej strony internetowej przy użyciu języka C!</p>
</body>
</html>
```

## Deep Dive

W przypadku, gdy chcemy pobrać treść z bardziej złożonej strony internetowej zawierającej pliki CSS czy JavaScript, możemy skorzystać z biblioteki `libcurl`, która oferuje dodatkowe funkcje do pobierania danych z internetu. Możemy również wykorzystać funkcję `fscanf()` do sparsowania pobranych danych zgodnie ze wzorcem.

## Zobacz też

- Dokumentacja języka C: https://www.gnu.org/software/gnu-c-manual/gnu-c-manual.html
- Biblioteka `stdio.h`: https://www.gnu.org/software/libc/manual/html_node/Standard-Streams.html
- Biblioteka `libcurl`: https://curl.haxx.se/libcurl/