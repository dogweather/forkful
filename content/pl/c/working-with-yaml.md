---
title:                "Praca z formatem yaml"
html_title:           "C: Praca z formatem yaml"
simple_title:         "Praca z formatem yaml"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/working-with-yaml.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek potrzebowałeś przechowywać dane w czytelny dla człowieka sposób, ale jednocześnie być w stanie je łatwo przetwarzać w swoim programie? Wtedy warto poznać YAML - czytelną dla ludzi strukturę danych, która jest również łatwa do interpretacji przez komputer. Przeczytaj nasz artykuł, aby dowiedzieć się, co to jest YAML i jak go używać w języku C.

## Jak używać YAML w języku C?

To proste! Musisz jedynie zaimportować bibliotekę libyaml do swojego projektu. Następnie możesz łatwo wczytywać i zapisywać dane w formacie YAML. Oto przykładowy kod:

```C
#include <yaml.h>

// Tworzymy nowy dokument YAML
yaml_document_t document;
yaml_document_initialize(&document, NULL, NULL, NULL, 0, 0);

// Dodajemy sekcję z kluczem "zawartość" i wartością "tekst"
yaml_node_t *content = yaml_document_add_scalar(&document, (yaml_char_t *)"zawartość", (yaml_char_t *)"tekst");

// Tworzymy przepływ wyjściowy do zapisu dokumentu w formacie YAML
yaml_emitter_t emitter;
yaml_emitter_initialize(&emitter);

// Ustawiamy standardowy strumień wyjściowy na stdout
yaml_emitter_set_output_file(&emitter, stdout);

// Emitujemy nasz dokument do strumienia wyjściowego
yaml_emitter_dump(&emitter, &document);

// Zwalniamy zasoby
yaml_document_delete(&document);
yaml_emitter_delete(&emitter);

// Output:
// zawartość: tekst
```

## Pogłębiona analiza

Dzięki YAML możesz przechowywać złożone struktury danych, takie jak tablice i słowniki (klucz-wartość). Ponadto, ten format jest niezwykle czytelny dla człowieka, co ułatwia jego modyfikację i aktualizację.

W języku C dostępna jest biblioteka libyaml, która umożliwia proste parsowanie i generowanie dokumentów w formacie YAML. Warto zapoznać się z jej dokumentacją dla pełnej listy funkcji i możliwości.

## Zobacz również

- Dokumentacja dla biblioteki libyaml (https://pyyaml.org/wiki/LibYAML)
- Przykładowe projekty wykorzystujące YAML i język C (https://github.com/yaml/libyaml#projects-using-libyaml)
- Tutorial na temat obsługi formatu YAML w języku C (https://www.commsp.ee.ic.ac.uk/~vijay/nbook/booklpsoct.html)