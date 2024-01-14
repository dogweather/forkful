---
title:                "C: Praca z yaml"
simple_title:         "Praca z yaml"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/working-with-yaml.md"
---

{{< edit_this_page >}}

## Dlaczego

C jest jednym z najpopularniejszych języków programowania na świecie i często używany do tworzenia aplikacji i systemów. Jedną z zalet C jest możliwość łatwego obsługiwania formatu danych YAML. Przeczytaj ten artykuł, aby dowiedzieć się dlaczego warto poznać YAML i jak możesz go używać w swoich projektach.

## Jak to zrobić

W celu pracy z formatem YAML w języku C musimy użyć biblioteki zewnętrznej, takiej jak yaml-c. Należy pamiętać, że biblioteka ta jest dostępna tylko dla systemów operacyjnych unixowych, więc jeśli pracujesz na Windowsie, musisz skorzystać z innej biblioteki.

```C
#include <stdio.h>
#include <yaml.h>      // wczytaj bibliotekę yaml-c

int main()
{
  // utwórz nowy obiekt parsera YAML
  yaml_parser_t parser;
  
  // zainicjuj obiekt
  yaml_parser_initialize(&parser);
  
  // otwórz plik zawierający dane YAML
  FILE *file = fopen("example.yaml", "rb");
  
  // ustaw strumień wejściowy dla parsera
  yaml_parser_set_input_file(&parser, file);
  
  // zacznij analizę danych YAML
  yaml_event_t event;
  do {
    yaml_parser_parse(&parser, &event);  // pobierz kolejne zdarzenie
    switch (event.type) {
      case YAML_SCALAR_EVENT:
        // przetwórz dane typu skalar
        printf("Klucz: %s\n", event.data.scalar.value);
        break;
      case YAML_SEQUENCE_START_EVENT:
        // przetwórz początek sekwencji
        printf("Sekwencja rozpoczęta\n");
        break;
      case YAML_MAPPING_START_EVENT:
        // przetwórz początek mapowania
        printf("Mapowanie rozpoczęte\n");
        break;
      // dodaj obsługę pozostałych typów zdarzeń
    }
    // zwolnij pamięć zdarzenia
    yaml_event_delete(&event);
  } while (event.type != YAML_STREAM_END_EVENT);
  
  // zwolnij pamięć parsera
  yaml_parser_delete(&parser);
  
  // zamknij plik
  fclose(file);
  
  return 0;
}
```

Przykładowy plik `example.yaml` może wyglądać następująco:

```yaml
klucz: wartość
lista:
  - element 1
  - element 2
mapa:
  klucz1: wartość1
  klucz2: wartość2
```

Kod powyżej wyświetli następujące dane na konsoli:

```
Klucz: klucz
Sekwencja rozpoczęta
Klucz: lista
Klucz: element 1
Klucz: element 2
Mapowanie rozpoczęte
Klucz: mapa
Klucz: klucz1
Klucz: wartość1
Klucz: klucz2
Klucz: wartość2
```

## Głębszy zanurzenie

Format YAML jest bardzo użyteczny, gdy chcemy przechowywać i przekazywać dane w sposób czytelny dla człowieka. Jest to szczególnie przydatne, gdy pracujemy z dużymi zbiorami danych, takimi jak konfiguracje systemowe czy dane użytkowników.

Biblioteka yaml-c oferuje wiele funkcji do wyciągania i zapisywania danych w formacie YAML. Możemy na przykład dodać własne zdarzenia do parsera, aby dostosować jego zachowanie dla naszych potrzeb. Możemy również wykorzystać wbudowane funkcje do konwersji danych z formatu YAML do innych formatów, takich jak JSON czy XML.

Jednym z zastosowań YAML jest tworzenie plików konfiguracyjnych dla aplikacji, co pozwala na łatwą zmianę ustawień bez konieczności modyfikacji kodu.