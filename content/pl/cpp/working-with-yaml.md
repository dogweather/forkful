---
title:                "C++: Praca z plikiem yaml"
simple_title:         "Praca z plikiem yaml"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą lub chcesz zostać nim w przyszłości, na pewno słyszałeś o języku YAML. Jest to popularny format, który jest wykorzystywany do przechowywania i przekazywania danych. Ale dlaczego warto poznać ten język? Dlaczego warto nauczyć się go używać? Przeczytaj ten artykuł, aby dowiedzieć się dlaczego YAML jest tak ważny w świecie programowania.

## Jak zacząć

Jeśli chcesz zacząć pracę z YAML, musisz najpierw zainstalować odpowiednie biblioteki i narzędzia. W języku C++, popularnym wyborem jest biblioteka libyaml, która zapewnia wygodne API do manipulacji plikami YAML. Poniżej przedstawiamy krótki przykład kodu, który pokazuje jak zaimportować i wykorzystać tę bibliotekę.

```C++
#include <yaml.h>

int main() {
    // Tworzenie obiektu parsera
    yaml_parser_t parser;
    if(!yaml_parser_initialize(&parser)) {
        // Błąd podczas inicjalizacji
        return 0;
    }

    // Powiązanie źródła z parserem
    FILE *file = fopen("example.yaml", "rb");
    yaml_parser_set_input_file(&parser, file);

    // Pętla parsująca
    do {
        // Pobieranie następnego elementu
        yaml_event_t event;
        yaml_parser_parse(&parser, &event);

        // Wykonywanie odpowiednich operacji w zależności od typu elementu
        // ...

        // Zwalnianie pamięci używanej przez event
        yaml_event_delete(&event);
    } while(!yaml_parser_event_done(&parser, &event));

    // Zwolnienie parsera
    yaml_parser_delete(&parser);

    return 0;
}
```

Powyższy kod przedstawia zestawienie podstawowych funkcji, które używane są podczas pracy z YAML. Ważne jest, aby pamiętać o zwalnianiu pamięci przy użyciu odpowiednich funkcji, aby uniknąć wycieków pamięci.

## Głębszy zanurzenie

Jeśli chcesz nauczyć się więcej o pracy z YAML, musisz poznać jego strukturę. Poniżej przedstawiamy krótki opis głównych elementów, które należy znać:

- Kontenery - służą do grupowania elementów wewnętrznych, takich jak tablice (ang. arrays) czy słowniki (ang. dictionaries).
- Skalary - reprezentują pojedyncze wartości, na przykład liczby, ciągi znaków lub logiczne wartości.
- Sekwencje - uporządkowana lista elementów. Sekwencje zapisywane są przy użyciu myślnika (-) przed każdym elementem.
- Mapy - nieuporządkowana lista klucz-wartość. Mapy zapisywane są przy użyciu dwukropka (:) pomiędzy kluczem a wartością.

Dzięki połączeniu powyższych elementów możesz tworzyć struktury danych w plikach YAML, które będą czytelne i łatwe do przetworzenia przez program.

## Zobacz również

- [Dokumentacja biblioteki libyaml](https://pyyaml.org/wiki/LibYAML)
- [Animator online do tworzenia plików YAML](https://www.yamllint.com/)
- [Poradnik pracy z YAML dla początkujących](https://www.tutorialspoint.com/yaml/index.htm)