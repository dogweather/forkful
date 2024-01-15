---
title:                "Praca z formatem json"
html_title:           "Go: Praca z formatem json"
simple_title:         "Praca z formatem json"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/working-with-json.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego warto nauczyć się pracować z JSON w Go? Ponieważ JSON jest popularnym formatem danych, a Go jest szybkim i wydajnym językiem programowania. Dzięki nauce pracy z JSON w Go będziesz w stanie łatwo obsługiwać i przetwarzać dane w swoich projektach.

## Jak to zrobić

Poniżej przedstawiam prosty przykład kodu w Go, który pokazuje jak pracować z JSON:

```Go
package main

import (
    "encoding/json"
    "fmt"
)

type Person struct {
    Name string `json:"name"`
    Age  int    `json:"age"`
}

func main() {
    jsonStr := `{"name": "Kasia", "age": 25}`
    var p Person
    err := json.Unmarshal([]byte(jsonStr), &p)
    if err != nil {
        panic(err)
    }
    fmt.Println("Imię:", p.Name)
    fmt.Println("Wiek:", p.Age)
}
```

W powyższym przykładzie, zaczynamy od zdefiniowania struktury `Person`, która będzie odpowiadać danej osoby w formacie JSON. Następnie, używamy funkcji `Unmarshal` z pakietu `encoding/json` aby przetworzyć dane zmienną `jsonStr` do zmiennej `p` zgodnej z naszą strukturą. Na końcu, wypisujemy imię i wiek osoby na ekran.

Output:

```
Imię: Kasia
Wiek: 25
```

## Deep Dive

Do głębszego poznania pracy z JSON w Go warto przeczytać dokumentację pakietu `encoding/json`. W tym pakiecie znajdują się funkcje takie jak `Marshal` i `Unmarshal`, które pozwalają na konwersję danych JSON do danych Go oraz na odwrócenie tego procesu. Możesz także dowiedzieć się więcej o tagach struktury, które są używane do mapowania danych JSON na pola struktury Go.

## Zobacz także

- [Dokumentacja pakietu encoding/json](https://golang.org/pkg/encoding/json/)
- [Przykłady użycia JSON w Go](https://gobyexample.com/json)
- [Wprowadzenie do pracy z JSON w Go](https://medium.com/@jaturongkubert/json-encode-decode-in-golang-6d5e5b422351)