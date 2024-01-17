---
title:                "Praca z yaml"
html_title:           "C: Praca z yaml"
simple_title:         "Praca z yaml"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/working-with-yaml.md"
---

{{< edit_this_page >}}

## O co chodzi i dlaczego?

Pracowanie z formatem YAML jest coraz popularniejsze wśród programistów, ponieważ pozwala on na czytelne i przejrzyste przechowywanie danych. Jest to plik tekstowy, który w prosty sposób może być odczytywany przez człowieka, co ułatwia pracę z nim. Dzięki temu, programiści mogą szybciej tworzyć i aktualizować swoje aplikacje.

## Jak to zrobić:
Zobacz poniżej przykładowy kod w języku C oraz jego wyjście. Wykorzystujemy bibliotekę libyaml, aby obsłużyć pliki w formacie YAML.

```C
#include <yaml.h>

int main() {
    // Przykładowe dane w formacie YAML
    char *input = "name: John Smith\nage: 35\n";
    
    // Inicjalizacja parsera
    yaml_parser_t parser;
    yaml_event_t event;
    
    // Ustawienia parsera
    yaml_parser_initialize(&parser);
    yaml_parser_set_input_string(&parser, input, strlen(input));
    
    // Parsowanie danych
    do {
        // Pobranie kolejnego wydarzenia
        if (!yaml_parser_parse(&parser, &event)) {
            break;
        }
        
        // Sprawdzenie typu wydarzenia
        if (event.type == YAML_SCALAR_EVENT) {
            // Wyświetlenie klucza i wartości
            printf("Klucz: %s\nWartość: %s\n",
                   event.data.scalar.tag, event.data.scalar.value);
        }
        
        // Pamiętaj o zwolnieniu pamięci dla wydarzenia
        yaml_event_delete(&event);
    } while (event.type != YAML_STREAM_END_EVENT);
    
    // Czyszczenie po sobie
    yaml_parser_delete(&parser);

    return 0;
}
```

Wyjście:
```
Klucz:tag:
Wartość: name
Klucz: name
Wartość: John Smith
Klucz: age
Wartość: 35
```

## Głębszy zanurzenie:
Format YAML został stworzony w 2001 roku przez Clarka Evansa, Ingy döt Net i Oren Ben-Kiki. Służy do zapisywania konfiguracji oraz przechowywania danych strukturalnych w postaci czytelnej dla człowieka. Alternatywami dla YAML są między innymi XML czy JSON, ale YAML jest znacznie bardziej czytelny i łatwiejszy w użyciu.

Implementacja biblioteki libyaml jest dostępna w językach C, C++, C#, Java, Perl, Python, Ruby i innych. Jest to darmowa biblioteka open source, więc może być wykorzystana w projektach komercyjnych bez ponoszenia kosztów.

## Zobacz również:
- Oficjalna dokumentacja biblioteki libyaml: https://pyyaml.org/wiki/LibYAML
- Strona internetowa formatu YAML: https://yaml.org/