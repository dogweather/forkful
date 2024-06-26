---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:12:09.750006-07:00
description: "Jak to zrobi\u0107: W Go, pakiet `encoding/json` jest Twoj\u0105 bram\u0105\
  \ do manipulacji JSONem, zapewniaj\u0105c mechanizmy konwersji struktur danych Go\
  \ do JSON\u2026"
lastmod: '2024-03-13T22:44:34.876739-06:00'
model: gpt-4-0125-preview
summary: "W Go, pakiet `encoding/json` jest Twoj\u0105 bram\u0105 do manipulacji JSONem,\
  \ zapewniaj\u0105c mechanizmy konwersji struktur danych Go do JSON (marshalling)\
  \ i z powrotem (unmarshalling)."
title: Praca z JSON
weight: 38
---

## Jak to zrobić:
W Go, pakiet `encoding/json` jest Twoją bramą do manipulacji JSONem, zapewniając mechanizmy konwersji struktur danych Go do JSON (marshalling) i z powrotem (unmarshalling). Poniżej znajdują się podstawowe przykłady, które pomogą Ci zacząć:

### Kodowanie (Marshalling)
Aby przekonwertować strukturę Go na JSON, możesz użyć `json.Marshal`. Rozważ następującą strukturę Go:

```go
package main

import (
    "encoding/json"
    "fmt"
    "log"
)

type User struct {
    ID        int      `json:"id"`
    Username  string   `json:"username"`
    Languages []string `json:"languages"`
}

func main() {
    user := User{1, "JohnDoe", []string{"Go", "JavaScript", "Python"}}
    userJSON, err := json.Marshal(user)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Println(string(userJSON))
}
```

Wynik:

```json
{"id":1,"username":"JohnDoe","languages":["Go","JavaScript","Python"]}
```

### Dekodowanie (Unmarshalling)
Aby przeanalizować JSON do struktury danych Go, użyj `json.Unmarshal`:

```go
package main

import (
    "encoding/json"
    "fmt"
    "log"
)

func main() {
    jsonStr := `{"id":1,"username":"JohnDoe","languages":["Go","JavaScript","Python"]}`
    var user User
    err := json.Unmarshal([]byte(jsonStr), &user)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Printf("%+v\n", user)
}
```

Przy wyżej wymienionej strukturze `User`, ten kod analizuje ciąg JSON do instancji User.

Wynik:

```go
{ID:1 Username:JohnDoe Languages:[Go JavaScript Python]}
```

## Pogłębiona analiza
Pakiet `encoding/json` w Go oferuje prosty API, który abstrahuje wiele złożoności związanych z manipulacją JSON. Wprowadzony we wczesnej fazie rozwoju Go, ten pakiet odzwierciedla filozofię Go dotyczącą prostoty i efektywności. Jednak użycie refleksji przez `encoding/json` do inspekcji i modyfikacji struktur w czasie rzeczywistym może prowadzić do mniejszej niż optymalna wydajność w scenariuszach intensywnie korzystających z CPU.

Alternatywy takie jak `json-iterator/go` i `ffjson` pojawiły się, oferując szybsze przetwarzanie JSON poprzez generowanie statycznego kodu marshallingu i unmarshallingu. Jednakże, `encoding/json` pozostaje najczęściej używanym pakietem ze względu na swoją prostotę, solidność i fakt, że jest częścią standardowej biblioteki, zapewniając zgodność i stabilność przez różne wersje Go.

Pomimo jego względnie wolniejszej wydajności, łatwość użycia i integracja z systemem typów Go sprawiają, że `encoding/json` nadaje się do większości aplikacji. Dla tych, którzy pracują w kontekstach, gdzie wydajność jest kluczowa, zbadanie zewnętrznych bibliotek może być warte rozważenia, ale dla wielu standardowa biblioteka osiąga właściwą równowagę między szybkością, prostotą i niezawodnością.
