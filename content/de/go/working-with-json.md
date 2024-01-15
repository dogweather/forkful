---
title:                "Arbeiten mit JSON"
html_title:           "Go: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/working-with-json.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man mit JSON arbeiten? Ganz einfach: JSON ist ein einfaches und effizientes Datenformat, das in der heutigen webbasierten Entwicklung weit verbreitet ist. Es ermöglicht die Übertragung und Speicherung von strukturierten Daten auf einfache und menschenlesbare Weise.

## Wie geht es?

Um mit JSON in Go zu arbeiten, müssen Sie zuerst das `encoding/json` Paket importieren. Dann können Sie Ihre Datenstrukturen wie folgt definieren:

```Go
type Person struct {
    Name string `json:"name"`
    Age int `json:"age"`
}
```

Der Schlüssel `json` in den Feldern legt fest, wie das JSON Objekt benannt werden soll. Jetzt können Sie ein JSON Objekt aus einer Person-Instanz erstellen und es codieren:

```Go
func main() {
    p := Person{Name: "Max", Age: 25}
    b, err := json.Marshal(p)

    if err != nil {
        fmt.Println("Fehler beim Codieren: ", err)
    }

    // b ist ein []byte Objekt mit den JSON Daten
    fmt.Println(string(b))
}
```

Die Ausgabe dieses Beispiels wäre:

```JSON
{"name": "Max", "age": 25}
```

Um ein JSON Objekt zu dekodieren und in eine Go Struktur zu bringen, verwenden Sie `json.Unmarshal()`:

```Go
func main() {
    b := []byte(`{"name": "Max", "age": 25}`)
    var p Person
    err := json.Unmarshal(b, &p)

    if err != nil {
        fmt.Println("Fehler beim Dekodieren: ", err)
    }

    fmt.Println("Name:", p.Name, "Alter:", p.Age)
}
```

Die Ausgabe dieser Codezeilen wäre:

``` 
Name: Max Alter: 25
```

## Deep Dive

Manchmal kann es vorkommen, dass Sie mit komplexen JSON Strukturen arbeiten müssen, die aus mehreren verschachtelten Objekten bestehen. In solchen Fällen kann es hilfreich sein, die Funktion `json.RawMessage` zu verwenden, um das decodierte JSON in ein Byte-Array zu speichern. Dies ermöglicht es Ihnen, das JSON später zu analysieren und die benötigten Daten auszuwählen.

Eine weitere nützliche Funktion ist `json.Decoder`, mit der Sie ein JSON Objekt schrittweise lesen und dekodieren können, anstatt es auf einmal zu laden.

## Siehe auch

- Offizielle Dokumentation des `encoding/json` Pakets: https://golang.org/pkg/encoding/json/
- Eine Einführung in JSON mit Go: https://blog.golang.org/json-and-go