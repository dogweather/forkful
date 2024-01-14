---
title:                "Go: Ein Datum in eine Zeichenfolge umwandeln"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum
Das Konvertieren von Datumsangaben in eine Zeichenfolge ist ein häufiger Bedarf in der Entwicklung, sei es für die Anzeige von Daten in einer Benutzeroberfläche oder für den Export in ein bestimmtes Format. In diesem Blog-Beitrag werden wir uns ansehen, wie wir dies mit Go erreichen können.

## Wie
Um ein Datum in eine Zeichenfolge umzuwandeln, können wir die Formatierungsfunktion in Go verwenden. Hier ist ein Beispiel, wie wir das aktuelle Datum in das Format "Tag.Monat.Jahr" umwandeln können:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	currentTime := time.Now()
	formattedTime := currentTime.Format("02.01.2006")
	fmt.Println(formattedTime)
}
```

Das Ergebnis dieses Codes wird in der Konsole wie folgt angezeigt:

`08.10.2021`

Wie Sie sehen können, haben wir die Funktion `Format` verwendet und das gewünschte Format als Parameter übergeben. Hier sind weitere Parameter, die für die Formatierung von Datumsangaben verwendet werden können:

- `Monat`: Monatsangabe, z.B. "Januar"
- `Mon`: Abkürzung für Monat, z.B. "Jan"
- `Tag`: Tagesangabe, z.B. "02"
- `Jahr`: Vierstellige Jahreszahl, z.B. "2021"
- `Jahr kurz`: Zwei-stellige Jahreszahl, z.B. "21"

Sie können auch mehrere Parameter kombinieren, um ein spezifischeres Format zu erreichen. Hier ist ein Beispiel, bei dem wir das aktuelle Datum und die Uhrzeit in das Format "Tag.Monat.Jahr Stunden:Minuten:Sekunden" umwandeln:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	currentTime := time.Now()
	formattedTime := currentTime.Format("02.01.2006 15:04:05")
	fmt.Println(formattedTime)
}
```

Das Ergebnis wird wie folgt angezeigt:

`08.10.2021 13:30:00`

## Deep Dive
Die `Format`-Funktion verwendet das "Time Format Dataset", das am 2. Januar 2006, 15:04:05 Uhr MST (Mountain Standard Time) definiert wurde. Der Grund dafür ist, dass dieses Datum und diese Uhrzeit leicht zu merken sind und es somit einfacher ist, Formate zu erstellen. Wenn Sie weitere Informationen zu den möglichen Formaten benötigen, können Sie die offizielle Dokumentation von Go zurate ziehen.

Es ist auch wichtig zu beachten, dass die `Format`-Funktion nur für Datumsangaben verwendet werden kann, die als `time.Time`-Objekt vorliegen. Wenn Sie ein Datum in Form einer Zeichenfolge oder eines anderen Datentyps haben, müssen Sie es zunächst in ein `time.Time`-Objekt umwandeln, bevor Sie es formatieren können.

## Siehe auch
- Offizielle Dokumentation zu Formatierungsanweisungen: https://golang.org/pkg/time/#Time.Format
- Zeitpakete in Go: https://golang.org/pkg/time/
- Umwandeln von Zeichenfolgen in Zeitobjekte in Go: https://yourbasic.org/golang/convert-string-to-time/