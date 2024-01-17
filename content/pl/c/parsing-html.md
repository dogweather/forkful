---
title:                "Analizowanie html"
html_title:           "C: Analizowanie html"
simple_title:         "Analizowanie html"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/parsing-html.md"
---

{{< edit_this_page >}}

## Co to jest i po co?

Parseowanie HTML to proces analizowania strony internetowej i wyodrębniania z niej informacji, takich jak tytuł, nagłówki czy teksty. Programiści często korzystają z tej techniki, aby automatyzować zadania związane z gromadzeniem danych ze stron internetowych.

## Jak to zrobić?

```
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
  // przykładowy kod
  char html[] = "<html><head><title>Tytuł strony</title></head><body><h1>Nagłówek</h1><p>Treść</p></body></html>";
  char *title = strstr(html, "<title>"); // wyszukaj tag <title>
  title += strlen("<title>"); // przejdź do początku tytułu
  char *endTitle = strstr(html, "</title>"); // wyszukaj koniec tagu <title>
  *endTitle = '\0'; // ustaw znak końca stringa
  printf("%s", title); // wyświetl tytuł: "Tytuł strony"
  
  char *header = strstr(html, "<h1>"); // wyszukaj tag <h1>
  header += strlen("<h1>"); // przejdź do początku nagłówka
  char *endHeader = strstr(html, "</h1>"); // wyszukaj koniec tagu <h1>
  *endHeader = '\0'; // ustaw znak końca stringa
  printf("%s", header); // wyświetl nagłówek: "Nagłówek"
  
  char *paragraph = strstr(html, "<p>"); // wyszukaj tag <p>
  paragraph += strlen("<p>"); // przejdź do początku tekstu
  char *endParagraph = strstr(html, "</p>"); // wyszukaj koniec tagu <p>
  *endParagraph = '\0'; // ustaw znak końca stringa
  printf("%s\n", paragraph); // wyświetl tekst: "Treść"
  
  return 0;
}
```

### Deep Dive

Parseowanie HTML pojawiło się w latach 90. wraz z rozwojem internetu. Jego popularność wzrosła dzięki potrzebie automatyzacji procesów związanych z przetwarzaniem informacji z internetu. Obecnie istnieją także inne metody analizowania stron, takie jak web scraping czy wykorzystywanie API, jednak parsowanie HTML wciąż znajduje zastosowanie w wielu projektach.

### Zobacz także

- [Dokumentacja języka C](https://docs.microsoft.com/en-us/cpp/c-language/cpp-c-language-reference)
- [Inne sposoby na analizowanie stron internetowych](https://www.scrapinghub.com/web-scraping-vs-api-whats-the-difference/)