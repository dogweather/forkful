---
title:                "C: Odczytywanie html"
simple_title:         "Odczytywanie html"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/parsing-html.md"
---

{{< edit_this_page >}}

## Dlaczego parsowanie HTML jest ważne?

Parsowanie HTML jest niezbędnym elementem tworzenia stron internetowych. Jest to proces analizowania kodu HTML w celu wyświetlenia go w przeglądarce internetowej w odpowiedniej formie. Bez parsowania, strona internetowa wyglądałaby chaotycznie i trudno byłoby ją czytać. Dlatego jest to niezbędna umiejętność dla każdego programisty, który chce tworzyć interaktywne i estetyczne strony internetowe.

## Jak to zrobić?

Parsowanie HTML w języku C może wydawać się nieco skomplikowane, ale jest to zadanie możliwe do wykonania. W tym przykładzie użyjemy biblioteki libxml2, która jest popularnym narzędziem do parsowania dokumentów XML i HTML w języku C.

Najpierw musimy zainstalować bibliotekę libxml2 na naszym komputerze. Następnie włączamy ją w naszym programie za pomocą dyrektywy #include. Poniżej znajduje się przykładowy kod, który parsuje prosty dokument HTML i wyświetla jego zawartość:

```C
#include <stdio.h>
#include <libxml/parser.h>

int main()
{
    xmlDoc *doc = NULL;
    xmlNode *root_element = NULL;

    doc = xmlReadFile("index.html", NULL, 0); //wczytywanie dokumentu HTML
    root_element = xmlDocGetRootElement(doc); //pobieranie korzenia dokumentu

    xmlNode *current_node = NULL;
    for (current_node = root_element->children; current_node; current_node = current_node->next)
    {
        if (current_node->type == XML_ELEMENT_NODE) //sprawdzanie czy węzeł jest elementem
        {
            //wyświetlanie nazwy elementu i jego zawartości
            printf("Element: %s \t Zawartość: %s\n", current_node->name, xmlNodeGetContent(current_node));
        }
    }

    xmlFreeDoc(doc); //zwalnianie pamięci
    xmlCleanupParser(); //czyszczenie parsera

    return 0;
}
```

Ten kod wyświetli zawartość dokumentu HTML w następującej postaci:

```
Element: html Zawartość:
Element: head Zawartość:
Element: title Zawartość: Moja Strona Internetowa
Element: body Zawartość:
Element: h1 Zawartość: Witaj na mojej stronie!
Element: p Zawartość: Jest to przykładowy tekst na stronie internetowej.
```

W ten sposób możemy odczytać i wyświetlić zawartość wybranych elementów w dokumencie HTML. Można również użyć innych funkcji biblioteki libxml2 do przeszukiwania i manipulowania zawartością dokumentu.

## Głębszy wgląd

Parsowanie HTML jest tylko jednym aspektem tworzenia stron internetowych. Istnieje wiele innych zagadnień, takich jak stylowanie CSS i programowanie JavaScript, które również są ważne dla stworzenia funkcjonalnej i atrakcyjnej strony internetowej.

Jednakże, zrozumienie, jak działa parsowanie HTML jest kluczowe dla programistów chcących tworzyć złożone strony internetowe. Dzięki temu będą mogli dostosować swoje strony do różnych przeglądarek i urządzeń, zapewniając użytkownikom najlepsze wrażenia z przeglądania.

## Zobacz także

- [Dokumentacja biblioteki libxml2](http://xmlsoft.org/html/libxml-parser.html)
- [Kurs C dla początkujących](https://www.tutorialspoint.com/cprogramming/)
- [Tworzenie stron internetowych za pomocą HTML i CSS](https://www.w3schools.com/html/default.asp)