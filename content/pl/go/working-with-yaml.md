---
title:                "Praca z yaml"
html_title:           "Go: Praca z yaml"
simple_title:         "Praca z yaml"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/working-with-yaml.md"
---

{{< edit_this_page >}}

# Co i Dlaczego?
Praca z YAML to nic innego jak obsługa pewnego rodzaju formatu danych w programowaniu. Programiści używają go do przechowywania ustrukturyzowanych informacji w plikach, które mogą być łatwo odczytywane przez ludzi i komputery.

# Jak to zrobić:
    ```Go
    // Zaimportuj pakiet yaml
    import "gopkg.in/yaml.v2"

    func main() {
        // Tworzenie mapy zawierającej przykładowe dane
        data := map[string]interface{}{
            "imie": "Anna",
            "wiek": "27",
            "hobby": "programowanie",
        }

        // Kodowanie mapy w formacie YAML
        output, err := yaml.Marshal(data)
        if err != nil {
            panic(err)
        }

        // Wyświetlanie wygenerowanego tekstu YAML
        fmt.Println(string(output))
    }
    ```
    Wynik:
    ```yaml
    imie: Anna
    wiek: 27
    hobby: programowanie
    ```
    
# Zagłębienie się:
- YAML (YAML Ain't Markup Language) został stworzony w 2001 roku przez Clarka Evansa, Ingy'ego döt Net i Oren Ben-Kikiego. Jest to bezpieczny, łatwy w czytaniu format danych, który jest często używany w konfiguracji aplikacji i komunikacji między aplikacjami.
- Alternatywami dla YAML są JSON i XML, jednak YAML jest bardziej czytelny dla człowieka i może łatwiej przetwarzać puste wartości i tablice wielowymiarowe.
- Biblioteka yaml.v2 jest wbudowana w język Go i udostępnia funkcje do kodowania i dekodowania danych w formacie YAML.

# Zobacz również:
- [Dokumentacja pakietu yaml.v2](https://pkg.go.dev/gopkg.in/yaml.v2)
- [Oficjalna strona YAML](https://yaml.org/)