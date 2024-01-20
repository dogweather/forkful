---
title:                "Analiza składniowa HTML"
html_title:           "Gleam: Analiza składniowa HTML"
simple_title:         "Analiza składniowa HTML"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/parsing-html.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Parsing HTML to proces interpretacji kodu HTML, czyli języka używanego do budowy stron internetowych. Programiści parsują HTML, aby mogli manipulować strukturą strony, analizować jej zawartość lub dostosować wygląd strony do konkretnej sytuacji.

## Jak to zrobić:

Podajemy prosty przykład zastosowania biblioteki Gumbo do parsowania HTML w C:

```C
#include <stdio.h>
#include <gumbo.h>

int main() {
  GumboOutput* output = gumbo_parse("<h1>Witaj Świecie</h1>");
  
  printf("Element główny: %s\n", gumbo_normalized_tagname(output->root->v.element.tag));
  
  gumbo_destroy_output(&kGumboDefaultOptions, output);
  return 0;
}
```

Gdy wykonamy ten program, zobaczymy output:

```C
Element główny: html
```

## W głąb tematu

Parsowanie HTML pojawiło się nieodłącznie z rozwojem internetu i potrzebą lepszego zrozumienia i manipulacji treścią stron WWW. Istnieją inne techniki parsowania, takie jak analiza składniowa XML czy JSON. Zależy to od zastosowania i struktury danych.

Gumbo, biblioteka którą użyliśmy w powyższym przykładzie, jest jednym z wielu narzędzi do parsowania HTML w C. Inni mogą preferować inne narzędzia, takie jak libxml2, w zależności od ich specyficznych potrzeb.

Gumbo interpretuje HTML i generuje drzewo parsowania zgodne z Modelem Obiektu Dokumentu, co pozwala łatwiej manipulować i analizować zawartość strony.

## Zobacz także:

1. Dokumentacja Gumbo: https://github.com/google/gumbo-parser
2. Wprowadzenie do dokumentów HTML i DOM: https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model/Introduction
3. Informatory o parsingu HTML w C: https://www.educba.com/c-html-parser/
4. Wszystko o HTML i XML: https://www.w3schools.com/whatis/whatis_html.asp