---
title:                "Att arbeta med json."
html_title:           "Go: Att arbeta med json."
simple_title:         "Att arbeta med json."
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/working-with-json.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att arbeta med JSON innebär att behandla data i ett format som är lättläst och lättbearbetat av både människor och datorer. Det är ett sätt för programmerare att strukturera och utbyta data på ett effektivt sätt.

## Så här:

```Go
import "encoding/json"

type Person struct {
    Name string `json:"name"`
    Age int `json:"age"`
    Address string `json:"address"`
}

func main() {
    person := Person{Name: "Anna", Age: 30, Address: "Stockholm"}

    // Skapa JSON från en struct
    jsonBytes, _ := json.Marshal(person)

    // Skriv ut JSON
    fmt.Println(string(jsonBytes))

    // Skapa en struct från JSON
    var newPerson Person
    newJson := `{"name":"Lisa","age":25,"address":"Malmö"}`
    json.Unmarshal([]byte(newJson), &newPerson)

    // Skriv ut den nya personen
    fmt.Println(newPerson.Name)
}
```

Output:

```
{"name":"Anna","age":30,"address":"Stockholm"}
Lisa
```

## Djupdykning:

JSON (JavaScript Object Notation) utvecklades på 2000-talet som ett lättläst och lättbearbetat alternativ till XML. Det används främst för datautbyte mellan program, webbtjänster och databaser. I Go är hanteringen av JSON inbyggd, men det finns även tredjepartsbibliotek tillgängliga.

För att strukturera data används vanligtvis JavaScript objekt och arrayer. Det finns också möjlighet att använda JSON Schema för att definiera en datamodell och validera JSON-data mot denna.

## Se även:

- https://golang.org/pkg/encoding/json/
- https://www.json.org/json-en.html