---
title:                "Arbeta med JSON"
html_title:           "Arduino: Arbeta med JSON"
simple_title:         "Arbeta med JSON"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/working-with-json.md"
---

{{< edit_this_page >}}

## Vad & Varför?
JSON hanteras för att utbyta data mellan system på ett enkelt och läsbart sätt. Programmerare gör det för att smidigt integrera olika delar av ett system, oavsett språk eller plattform.

## Så Gör Du:
```Go
package main

import (
    "encoding/json"
    "fmt"
    "log"
)

// Definiera en struct för att representera din data
type User struct {
    Name  string `json:"name"`
    Email string `json:"email"`
}

func main() {
    // Skapa en användare
    user := User{"Alice", "alice@example.com"}

    // Konvertera till JSON
    userJSON, err := json.Marshal(user)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Printf("%s\n", userJSON) // Output: {"name":"Alice","email":"alice@example.com"}

    // Läs JSON tillbaka till en struct
    jsonStr := `{"name":"Bob","email":"bob@example.com"}`
    var user2 User
    err = json.Unmarshal([]byte(jsonStr), &user2)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Printf("%+v\n", user2) // Output: {Name:Bob Email:bob@example.com}
}
```

## Djupdykning
JSON, eller JavaScript Object Notation, skapades tidigt 2000-tal och har blivit webbens de facto standard för datautbyte. Alternativen inkluderar XML och YAML, men JSON vinner i popularitet för sin enkelhet. Implementationen i Go sker genom `encoding/json`-paketet, som hanterar både omvandling från och till Go-strukturer och gör det lätt att jobba med JSON.

## Se Också
- Go dokumnetation om [JSON-paketet](https://pkg.go.dev/encoding/json)
- [JSON](https://www.json.org/json-en.html) officiell hemsida
- [Go By Example: JSON](https://gobyexample.com/json) för fler kodexempel