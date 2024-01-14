---
title:                "Go: Ein Datum in eine Zeichenfolge umwandeln"
simple_title:         "Ein Datum in eine Zeichenfolge umwandeln"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Warum

Go ist eine beliebte Programmiersprache, die für ihre Einfachheit und Effizienz bekannt ist. Eine der nützlichsten Funktionen in Go ist die Möglichkeit, ein Datum in einen String zu konvertieren. In diesem Blog-Beitrag werden wir uns genauer ansehen, warum und wie man das macht.

# Wie man ein Datum in einen String umwandelt


Um ein Datum in einen String umzuwandeln, gibt es in Go die Funktion `Format` in der `time`-Bibliothek. Hier ist ein Beispielcode, der das aktuelle Datum in verschiedenen Formaten ausgibt:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
    t := time.Now()
    fmt.Println("Datum in ISO 8601 Format:", t.Format("2006-01-02"))
    fmt.Println("Datum in amerikanischem Format:", t.Format("January 2, 2006"))
    fmt.Println("Datum in europäischem Format:", t.Format("2. Januar 2006"))
}
```

Die Ausgabe wird folgendermaßen aussehen:

```Go
Datum in ISO 8601 Format: 2021-03-10
Datum in amerikanischem Format: March 10, 2021
Datum in europäischem Format: 10. März 2021
```

Wie Sie sehen können, ist das Formatieren eines Datums in einen String in Go sehr einfach und flexibel. Denken Sie jedoch daran, dass das Formatieren immer auf den `Format`-Parameter in der Reihenfolge "Jahr - Monat - Tag" basiert.

# Tiefer Einblick

Nun, da wir wissen, wie man ein Datum in einen String umwandelt, lassen Sie uns einen tieferen Einblick in die `Format`-Funktion werfen. Diese Funktion erwartet zwei Parameter: das Format, in dem das Datum angezeigt werden soll, und das Datum selbst. Der `Format`-Parameter kann verschiedene Kombinationen von Zahlen und Buchstaben enthalten, die die spezifische Reihenfolge der Datumsbestandteile definieren.

Hier sind einige der gängigsten Zeichen und ihre Bedeutungen:

- `2006` steht für das Jahr
- `01` steht für den Monat, wenn er einstellig ist, und `1` für den Monat, wenn er zweistellig ist
- `02` steht für den Tag, wenn er einstellig ist, und `2` für den Tag, wenn er zweistellig ist
- `Januar` steht für den vollständigen Monatsnamen
- `Jan` steht für die abgekürzte Version des Monatsnamens

Es gibt auch viele andere Zeichen und Kombinationen, die Sie in der [Go-Dokumentation](https://golang.org/pkg/time/#Time.Format) nachschlagen können.

Ein weiterer wichtiger Punkt ist, dass die Funktion `Format` nur mit dem `time.Time`-Datentyp funktioniert. Daher müssen Sie ein Datum zuerst in dieses Format konvertieren, wenn Sie es formatieren möchten. Hier ist ein Beispiel für die Konvertierung eines Strings in ein `time.Time`-Objekt:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
    date := "2021-03-10"
    parsedDate, _ := time.Parse("2006-01-02", date)
    fmt.Println("Datum in europäischem Format:", parsedDate.Format("2. Januar 2006"))
}
```

Die Ausgabe wird folgendermaßen aussehen:

```Go
Datum in europäischem Format: 10. März 2021
```

Wie Sie sehen können, müssen wir zuerst das eingegebene Datum in das richtige Format konvertieren, bevor wir es formatieren können.

# Siehe auch

- [Go-Dokumentation zur `time`-Bibliothek](https://golang.org/pkg/time/)
- [Tutorium zu Go-Datumsformaten von Jon Calhoun](https://www.calhoun.io/working-with-date-and-time-in-go/)
- [A Comprehensive Guide to Dates and Times in Go von Jacob Martin](https://www.jacobmartins.com/2019/02/16/a-comprehensive-guide-to-dates-and-times-in-go/)