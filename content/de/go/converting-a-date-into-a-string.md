---
title:    "Go: Ein Datum in eine Zeichenfolge umwandeln."
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Konvertieren von Datumsangaben in Strings ist ein häufiges Problem, dem Entwickler in verschiedenen Anwendungsfällen begegnen. Zum Beispiel kann es erforderlich sein, ein Datum in einem bestimmten Format auszugeben, um es in einer Datenbank zu speichern oder als Teil eines Textes anzuzeigen. In diesem Beitrag werden wir uns ansehen, wie man in der Programmiersprache Go ein Datum in einen String konvertieren kann.

## Wie geht's

Die Konvertierung eines Datums in einen String ist in Go relativ einfach. Zunächst müssen wir die Funktion `Format` aus dem Paket `time` verwenden. Diese Funktion erwartet zwei Argumente: das angegebene Format und das Datum, das konvertiert werden soll. Im folgenden Beispiel konvertieren wir das aktuelle Datum in das Format "02.01.2006":

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	currentDate := time.Now()
	formattedDate := currentDate.Format("02.01.2006")
	fmt.Println(formattedDate)
}
```

Das Ergebnis wird in der Konsole als String ausgegeben und sieht folgendermaßen aus: "13.11.2021". Wir können auch die Funktion `Parse` verwenden, um ein Datum aus einem String in ein `time.Time`-Objekt zu konvertieren. Im folgenden Beispiel verwenden wir die gleiche Formatierung, um den String "13.11.2021" in ein `time.Time`-Objekt umzuwandeln:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	dateString := "13.11.2021"
	parsedDate, err := time.Parse("02.01.2006", dateString)
	if err != nil {
		panic(err)
	}
	fmt.Println(parsedDate)
}
```

Das Ergebnis ist das Datum als `time.Time`-Objekt, das für weitere Berechnungen oder Änderungen verwendet werden kann.

## Tiefer eintauchen

Wenn wir uns tiefer in die Thematik der Datumsformatierung in Go einarbeiten, werden wir feststellen, dass es viele weitere Möglichkeiten gibt, ein Datum in einen String umzuwandeln. Zum Beispiel können wir mit der Methode `Month()` eines `time.Time`-Objekts den Monatsnamen in verschiedenen Sprachen ausgeben. Wir können auch die Funktion `Format` nutzen, um die Zeitzone oder den Wochentag zu einem Datum hinzuzufügen.

Eine wichtige Sache zu beachten ist, dass das angegebene Format in der Funktion `Format` und `Parse` in bestimmten Teilen flexibel ist. Zum Beispiel können wir statt "02" auch "2", "2006" oder "06" verwenden, um das Jahr auszugeben. Eine vollständige Liste der unterstützten Formatierungen kann in der Dokumentation des Pakets `time` gefunden werden.

## Siehe auch

- [Go-Dokumentation zu `time`](https://golang.org/pkg/time/)
- [Go Playground mit Beispielen zur Datumsformatierung](https://play.golang.org/p/_bgaWf0tKTD)