---
title:                "Go: Extrahieren von Teilstrings"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/extracting-substrings.md"
---

{{< edit_this_page >}}

# Warum substrings extrahieren?

Das Extrahieren von Substrings ist eine nützliche Funktion in der Go-Programmierung. Es ermöglicht das Aufteilen von Strings in kleinere Teile und die gezielte Auswahl bestimmter Informationen. In diesem Blog-Beitrag werden wir uns ansehen, warum man Substrings extrahieren würde und wie man es in Go umsetzen kann.

## Wie man Substrings in Go extrahiert

Um Substrings in Go zu extrahieren, gibt es mehrere Möglichkeiten. Eine davon ist die Verwendung der `substring`-Funktion, die in der Standardbibliothek von Go enthalten ist. Hier ist ein Beispielcode mit der Verwendung der `substring`-Funktion:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "Hallo, das ist ein Beispieltext."
	substr := str[14:26]

	fmt.Println("Der extrahierte Substring ist:", substr) // Ausgabe: "Beispieltext."
}
```

In diesem Beispiel haben wir den Teil des Satzes extrahiert, der den Substring "Beispieltext" enthält, indem wir die Start- und Endpositionen angegeben haben. Beachten Sie, dass die Endposition nicht den tatsächlichen Index des letzten Zeichens im Substring, sondern den Index des Zeichens direkt dahinter angibt.

Eine weitere Möglichkeit, Substrings in Go zu extrahieren, ist die Verwendung von `strings.Split`, um den String anhand eines Trennzeichens in ein Array von Substrings zu zerlegen. Hier ist ein Beispielcode:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "Dies|ist|ein|Beispieltext."
	substr := strings.Split(str, "|")[3]

	fmt.Println("Der extrahierte Substring ist:", substr) // Ausgabe: "Beispieltext."
}
```

In diesem Beispiel haben wir den Substring "Beispieltext" extrahiert, indem wir das Trennzeichen "|" verwendet haben, um den String in ein Array von Substrings aufzuteilen. Da der Substring, den wir benötigen, an der 4. Stelle im Array steht, wählen wir einfach den entsprechenden Index aus.

## Tiefere Einblicke ins Extrahieren von Substrings

In Go gibt es auch die Möglichkeit, Substrings mit Hilfe von regulären Ausdrücken zu extrahieren. Reguläre Ausdrücke sind eine leistungsstarke Methode, um Muster in Strings zu finden und zu manipulieren. Ein Beispielcode für die Verwendung von regulären Ausdrücken in Go könnte so aussehen:

```Go
package main

import (
	"fmt"
	"regexp"
)

func main() {
	re := regexp.MustCompile(`H[a-z]+,`)
	str := "Hallo, dies ist ein Beispieltext."
	substr := re.FindString(str)

	fmt.Println("Der extrahierte Substring ist:", substr) // Ausgabe: "Hallo,"
}
```

In diesem Beispiel haben wir den Substring "Hallo," extrahiert, indem wir nach einem Muster gesucht haben, das mit einem "H" beginnt, gefolgt von einem oder mehreren Kleinbuchstaben und einem Komma. Beachten Sie, dass reguläre Ausdrücke in Go mit der `regexp`-Bibliothek verwendet werden und es verschiedene Methoden gibt, um Muster zu finden und zu manipulieren.

## Siehe auch

- Weitere Informationen zur `substring`-Funktion: https://golang.org/pkg/strings/#Substring
- Dokumentation über das `strings`-Paket: https://golang.org/pkg/strings/
- Tutorial zu regulären Ausdrücken in Go: https://www.digitalocean.com/community/tutorials/how-to-use-regular-expressions-in-go-de