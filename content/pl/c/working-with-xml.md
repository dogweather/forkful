---
title:                "Praca z XML"
date:                  2024-01-26T04:28:09.563558-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z XML"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/working-with-xml.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Praca z XML w języku C obejmuje parsowanie, tworzenie i manipulowanie plikami XML - zasadniczo strukturalne przechowywanie danych. Programiści robią to, aby interakcjiować z danymi w przenośnym i czytelnym dla człowieka formacie, często używanym do konfiguracji, wymiany danych i więcej.

## Jak to zrobić:
Poniżej znajduje się fragment kodu korzystający z biblioteki `libxml2` do parsowania pliku XML i pobierania elementu głównego.

```C
#include <stdio.h>
#include <libxml/parser.h>
#include <libxml/tree.h>

int main() {
    xmlDoc *doc = NULL;
    xmlNode *root_element = NULL;

    // Parsuj plik XML
    doc = xmlReadFile("example.xml", NULL, 0);

    // Pobierz element główny
    root_element = xmlDocGetRootElement(doc);

    printf("Element główny: %s\n", root_element->name);

    // Zwolnij dokument
    xmlFreeDoc(doc);

    // Oczyść parser
    xmlCleanupParser();

    return 0;
}
```

Przykładowe wyjście dla XML-a z głównym `<data>` mogłoby wyglądać:
```
Element główny: data
```

## Szczegółowe informacje
XML, czyli Rozszerzalny Język Znaczników, sięga końca lat 90. i zapewnia sposób opisania i strukturyzacji danych. W C, `libxml2` to pierwszy wybór. Jest solidna, choć nie najłatwiejsza dla początkujących w XML. Alternatywy to `tinyxml2`, która jest lżejsza i bardziej przyjazna dla początkujących. Jeśli chodzi o implementację, C nie ma wbudowanego wsparcia XML, więc biblioteki wypełniają tę lukę. Różnią się one rozmiarem, szybkością, złożonością i przenośnością. Większość oferuje metody parsowania DOM i SAX: DOM ładuje całość do pamięci, dobre dla małych dokumentów; SAX jest sterowany zdarzeniami, obsługuje elementy na bieżąco, lepszy dla dużych plików. Obie mają swoje przypadki użycia i kompromisy.

## Zobacz również
- [libxml2](http://xmlsoft.org/)
- [tinyxml2 na GitHubie](https://github.com/leethomason/tinyxml2)
- [Tutorial XML na w3schools](https://www.w3schools.com/xml/)
- [Specyfikacja XML przez W3C](https://www.w3.org/XML/)