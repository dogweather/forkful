---
title:                "Go: Praca z yaml"
simple_title:         "Praca z yaml"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/working-with-yaml.md"
---

{{< edit_this_page >}}

## Dlaczego

Praca z YAML jest nieodłączną częścią pracy programisty w języku Go. Wspiera on konfigurację i przechowywanie danych w aplikacjach, co pomaga w utrzymaniu czystego i czytelnego kodu. W tym artykule przeprowadzimy Cię przez podstawy pracy z YAML w środowisku Go.

## Jak to zrobić

Pierwszym krokiem jest zainstalowanie pakietu yaml.v2:

```Go
go get -u gopkg.in/yaml.v2
```

Następnie, zaimportuj go w swoim pliku Go:

```Go
import "gopkg.in/yaml.v2"
```

Teraz możesz zacząć pracę z plikami YAML. Poniżej znajduje się przykład kodu, który tworzy nowy plik YAML i zapisuje w nim dane:

```Go
func main() {
    data := map[string]interface{}{
        "imie": "Anna",
        "wiek": 30,
        "hobby": []string{"programowanie", "fotografia", "podróże"}
    }

    file, err := os.Create("dane.yaml")
    if err != nil {
        panic(err)
    }
    defer file.Close()

    encoder := yaml.NewEncoder(file)
    defer encoder.Close()

    err = encoder.Encode(data)
    if err != nil {
        panic(err)
    }
}
```

Powyższy kod stworzy plik "dane.yaml" z zawartością:

```yaml
imie: Anna
wiek: 30
hobby:
- programowanie
- fotografia
- podróże
```

## Głębsze zanurzenie

Praca z YAML będzie jeszcze bardziej wygodna, jeśli nauczysz się korzystać z funkcji unmarshal i marshal z pakietu yaml. Funkcja unmarshal pozwala na przekształcenie danych YAML do struktur w języku Go, a funkcja marshal służy do przekształcenia struktur w języku Go do danych YAML. Poniżej znajdują się przykładowe wykorzystania tych funkcji:

```Go
func main() {
    // przekształcenie danych YAML do struktury
    type Dane struct {
        Imie string `yaml:"imie"`
        Wiek int `yaml:"wiek"`
        Hobby []string `yaml:"hobby"`
    }

    var dane Dane

    data := `imie: Anna
    wiek: 30
    hobby:
    - programowanie
    - fotografia
    - podróże`

    err := yaml.Unmarshal([]byte(data), &dane)
    if err != nil {
        panic(err)
    }

    fmt.Println(dane) // {Anna 30 [programowanie fotografia podróże]}

    // przekształcenie struktury do danych YAML
    noweData, err := yaml.Marshal(&dane)
    if err != nil {
        panic(err)
    }

    fmt.Println(string(noweData)) // imie: Anna
                                  // wiek: 30
                                  // hobby:
                                  // - programowanie
                                  // - fotografia
                                  // - podróże
}
```

Ważne jest również, aby zwrócić uwagę na formatowanie danych w pliku YAML. Należy używać wcięć zamiast znaków tabulacji, co pomaga zachować czytelność kodu. Można również używać stringów do przypisywania kluczy, co pomaga uniknąć problemów z białymi znakami.

## Zobacz również

- Oficjalna dokumentacja pakietu yaml.v2: https://godoc.org/gopkg.in/yaml.v2
- Tutorial dotyczący pracy z YAML w Go: https://www.sohamkamani.com/blog/2017/10/18/parsing-json-in-golang/
- Narzędzie do przetwarzania danych YAML w języku Go: https://github.com/mitchellh/mapstructure