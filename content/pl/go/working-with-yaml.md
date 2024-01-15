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

## Dlaczego
Go (znany także jako Golang) jest jednym z najpopularniejszych języków programowania obecnie używanych. Jest wykorzystywany do tworzenia szybkich i efektywnych aplikacji internetowych, narzędzi systemowych, a nawet gier. Jedną z zalet Go jest jego wszechstronność i możliwość obsługi różnych formatów danych, w tym YAML. W tym artykule dowiesz się, dlaczego warto poznać Go w kontekście pracy z YAML.

## Jak
Aby rozpocząć pracę z YAML w Go, wystarczy importować odpowiedni pakiet. Następnie możesz użyć funkcji Unmarshal, aby wczytać dane z pliku YAML do struktury w Go lub użyć Marshal do zapisania danych z Go do formatu YAML. Poniżej znajdują się przykłady kodu, które pokazują te operacje w praktyce.

```Go
import (
    "fmt"
    "io/ioutil"

    "gopkg.in/yaml.v2"
)

// Struktura danych w Go
type Person struct {
    Name string `yaml:"name"`
    Age int `yaml:"age"`
}

func main() {
    // Otwórz plik YAML
    data, err := ioutil.ReadFile("person.yaml")
    if err != nil {
        panic(err)
    }

    // Stwórz nową instancję struktury Person i zadeklaruj zmienną do przechowywania danych z pliku YAML
    var p Person 
    // Wykorzystaj funkcję Unmarshal do wczytania danych z pliku do struktury Person
    err = yaml.Unmarshal(data, &p)
    if err != nil {
        panic(err)
    }

    // Wyświetl dane wczytane z pliku YAML
    fmt.Println("Imię:", p.Name)
    fmt.Println("Wiek:", p.Age)

    // Stwórz nową instancję struktury Person i przypisz wartości do jej pól
    john := Person{
        Name: "John Doe",
        Age: 35,
    }

    // Utwórz mapę do przechowywania danych w formacie YAML
    m := make(map[string]interface{})
    // Wykorzystaj funkcję Marshal, aby zapisać dane z instancji john do formatu YAML i przypisz do mapy m
    out, err := yaml.Marshal(&john)
    if err != nil {
        panic(err)
    }
    // Wyświetl dane w formacie YAML
    fmt.Println(string(out))

}
```

Output:

Imię: John Doe
Wiek: 35

name: John Doe
age: 35

## Deep Dive
Go oferuje także wiele innych funkcji związanych z pracą z YAML. Możesz na przykład używać tagów w strukturze danych, aby kontrolować nazwy pól w pliku YAML lub użyć funkcji Encoder i Decoder do bardziej zaawansowanej pracy z formatem YAML. W przypadku większych plików zaleca się także korzystanie z funkcji Decoder zamiast Unmarshal, ponieważ jest ona bardziej wydajna i nie wymaga wczytywania całego pliku na raz. Aby dowiedzieć się więcej o tych funkcjach, warto zapoznać się z dokumentacją biblioteki ``gopkg.in/yaml.v2``.

## Zobacz także
- Dokumentacja Go: https://golang.org/doc/
- Biblioteka YAML w Go: https://gopkg.in/yaml.v2