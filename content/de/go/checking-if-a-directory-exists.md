---
title:    "Go: Überprüfen, ob ein Verzeichnis existiert"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Warum

Wenn Sie mit der Go-Programmiersprache arbeiten, kommt es oft vor, dass Sie prüfen müssen, ob ein bestimmtes Verzeichnis auf Ihrem System existiert. Das kann verschiedene Gründe haben, zum Beispiel um sicherzustellen, dass Ihre Anwendung auf alle benötigten Dateien zugreifen kann oder um sicherzustellen, dass der Code in der richtigen Umgebung ausgeführt wird. In diesem Blogbeitrag erfahren Sie, wie Sie mithilfe von Go-Code überprüfen können, ob ein Verzeichnis existiert.

## Wie geht das?

Um zu überprüfen, ob ein Verzeichnis existiert, müssen Sie zuerst das Paket "os" importieren. Dieses Paket enthält Funktionen, die Ihnen dabei helfen, mit dem Betriebssystem zu interagieren. Im Folgenden finden Sie ein Beispiel:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	// Hier definieren wir den Pfad zu unserem zu überprüfenden Verzeichnis
	dir := "C:/Users/Max/Documents/"

	// Mithilfe der "Stat"-Funktion aus dem "os"-Paket können wir
	// überprüfen, ob dieses Verzeichnis existiert
	if _, err := os.Stat(dir); err != nil {
		// Wenn ein Fehler auftritt, bedeutet das, dass das Verzeichnis nicht existiert
		fmt.Printf("Das Verzeichnis '%s' existiert nicht\n", dir)
	} else {
		// Wenn kein Fehler auftritt, bedeutet das, dass das Verzeichnis existiert
		fmt.Printf("Das Verzeichnis '%s' existiert\n", dir)
	}
}
```

In diesem Beispiel verwenden wir die Funktion "Stat" aus dem "os"-Paket, um den Status eines Verzeichnisses abzufragen. Diese Funktion gibt zwei Werte zurück: den statischen Status und einen Fehlerwert. Wenn das Verzeichnis existiert, wird der erste Wert (der Status) zurückgegeben, andernfalls wird ein Fehler zurückgegeben. In unserem Beispiel prüfen wir einfach, ob ein Fehler zurückgegeben wird oder nicht.

Die Ausgabe für dieses Beispiel wäre:

```
Das Verzeichnis 'C:/Users/Max/Documents/' existiert
```

Falls das Verzeichnis nicht existiert, würde die Ausgabe lauten:

```
Das Verzeichnis 'C:/Users/Max/Documents/' existiert nicht
```

## Tiefere Einblicke

Wenn Sie sich eingehender mit der Funktionsweise von "os.Stat" beschäftigen möchten, können Sie die Dokumentation auf der offiziellen Go-Website (https://golang.org/pkg/os/#Stat) überprüfen. Dort finden Sie weitere Informationen über diese Funktion und deren Parameter.

Einer der häufigsten Fehler, die beim Überprüfen von Verzeichnissen auftreten, ist der Gebrauch von relativen Pfaden anstelle von absoluten Pfaden. Stellen Sie sicher, dass Sie immer den absoluten Pfad für das zu überprüfende Verzeichnis angeben, um unerwartete Fehler zu vermeiden.

## Siehe auch

- https://golang.org/pkg/os/
- https://golang.org/pkg/path/filepath/