---
title:                "Go: Ein String in Kleinschreibung umwandeln"
simple_title:         "Ein String in Kleinschreibung umwandeln"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

#Warum 

Das Konvertieren von Strings in Kleinbuchstaben ist in vielen Fällen nützlich, zum Beispiel bei der Eingabe von Benutzernamen oder Passwörtern, um sicherzustellen, dass Groß- und Kleinschreibung keine Rolle spielen.

#Wie geht das

Um einen String in Kleinbuchstaben umzuwandeln, können Sie die `strings.ToLower()` Funktion in Go verwenden. Hier ist ein Beispielcode, der einen String in Kleinbuchstaben konvertiert und das Ergebnis ausgibt:

```Go
package main

import "fmt"
import "strings"

func main() {
  myString := "Hallo Welt!"
  fmt.Println("Original String:", myString)
  lowerCaseString := strings.ToLower(myString)
  fmt.Println("In Kleinbuchstaben:", lowerCaseString)
}
```

Die Ausgabe dieses Codes wird wie folgt aussehen:

```
Original String: Hallo Welt!
In Kleinbuchstaben: hallo welt!
```

#Tief Tauchen

Beim Konvertieren von Strings in Kleinbuchstaben gibt es einige Dinge zu beachten. Zum einen wird der ursprüngliche String nicht geändert, stattdessen wird eine neue Kopie erstellt. Das bedeutet, dass Sie den konvertierten String in einer neuen Variablen speichern müssen, wenn Sie ihn weiterverwenden möchten.

Außerdem können manche Sprachen und Zeichenkodierungen beim Konvertieren von Groß- zu Kleinbuchstaben unterschiedliche Ergebnisse liefern. Es ist wichtig, die unterstützten Sprachen und Kodierungen in der `strings` Paketdokumentation zu überprüfen, um sicherzustellen, dass Ihr Code die gewünschten Ergebnisse liefert.

#Siehe auch

- [Go strings Paketdokumentation](https://golang.org/pkg/strings/)
- [Tutorial: Strings in Go](https://golangbyexample.com/strings-in-golang/)