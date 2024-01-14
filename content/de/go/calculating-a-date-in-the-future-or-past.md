---
title:                "Go: Berechnung eines Datums in der Zukunft oder Vergangenheit."
simple_title:         "Berechnung eines Datums in der Zukunft oder Vergangenheit."
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum
Das Berechnen von Datumsangaben in der Zukunft oder Vergangenheit kann für verschiedene Anwendungen nützlich sein, wie zum Beispiel die Planung von Terminen oder das Verfolgen von Deadlines.

## Wie man es macht
Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, können wir die `time`-Package in Go verwenden. Um ein Beispiel zu sehen, schauen wir uns den folgenden Code an:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	today := time.Now()
	tenDaysLater := today.AddDate(0, 0, 10)
	fmt.Println("In 10 Tagen ist der", tenDaysLater.Format("02.01.2006"))
}
```

Die Ausgabe dieses Codes wäre: `In 10 Tagen ist der 20.10.2021`. Wir verwenden die Methode `AddDate()` um `tenDaysLater` zu berechnen, indem wir 0 Jahre und 0 Monate hinzufügen und 10 Tage hinzufügen. Wir können auch ein Datum in der Vergangenheit berechnen, indem wir negative Zahlen verwenden.

## Tiefere Einblicke
Die `time`-Package bietet viele weitere Funktionen und Methoden, die beim Arbeiten mit Datumsangaben hilfreich sein können. Zum Beispiel können wir mit der `Parse()` Methode Datumsangaben in unterschiedlichen Formaten parsen oder mit der `Since()` Methode die Zeitdauer zwischen zwei Datumsangaben berechnen. Es lohnt sich, die Dokumentation der `time`-Package genauer anzusehen, um alle Möglichkeiten zu entdecken.

## Siehe auch
- [Go Zeitdokumentation] (https://golang.org/pkg/time/)
- [Go Zeitberechnung Beispielcode] (https://gobyexample.com/time)
- [Go Zeitparsing Beispielcode] (https://yourbasic.org/golang/parse-string-time-date-go/)