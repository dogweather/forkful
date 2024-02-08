---
title:                "Protokollierung"
aliases:
- de/go/logging.md
date:                  2024-02-03T17:59:02.880278-07:00
model:                 gpt-4-0125-preview
simple_title:         "Protokollierung"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/logging.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Das Protokollieren in der Softwareentwicklung ist der Prozess der Aufzeichnung von Informationen über die Ausführung eines Programms, um sein Verhalten zu verfolgen und Probleme zu diagnostizieren. Programmierer implementieren die Protokollierung, um die Softwareleistung zu überwachen, Fehler zu debuggen und die Systemsicherheit und -konformität zu gewährleisten, was sie zu einem unverzichtbaren Werkzeug für die Anwendungswartung und -analyse macht.

## Wie geht das:

In Go kann das Protokollieren mithilfe des Standardbibliothekspakets `log` implementiert werden. Dieses Paket bietet einfache Protokollierungsfunktionen, wie das Schreiben in die Standardausgabe oder in Dateien. Beginnen wir mit einem grundlegenden Beispiel für die Protokollierung in die Standardausgabe:

```go
package main

import (
	"log"
)

func main() {
	log.Println("Dies ist ein grundlegender Protokolleintrag.")
}
```

Ausgabe:
```
2009/11/10 23:00:00 Dies ist ein grundlegender Protokolleintrag.
```

Der Zeitstempel am Anfang des Protokolleintrags wird automatisch vom `log`-Paket hinzugefügt. Als Nächstes erkunden wir, wie man in eine Datei statt in die Standardausgabe protokolliert:

```go
package main

import (
	"log"
	"os"
)

func main() {
	file, err := os.OpenFile("app.log", os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0666)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	log.SetOutput(file)
	log.Println("Dieser Protokolleintrag geht in eine Datei.")
}
```

Nun implementieren wir einen fortgeschritteneren Anwendungsfall: die Anpassung des Protokollierungsformats. Go ermöglicht es Ihnen, einen benutzerdefinierten Logger mit `log.New()` zu erstellen:

```go
package main

import (
	"log"
	"os"
)

func main() {
	logger := log.New(os.Stdout, "CUSTOM LOG: ", log.Ldate|log.Ltime|log.Lshortfile)
	logger.Println("Dies ist eine benutzerdefinierte Protokollnachricht.")
}
```

Ausgabe:
```
CUSTOM LOG: 2009/11/10 23:00:00 main.go:11: Dies ist eine benutzerdefinierte Protokollnachricht.
```

Dieses Beispiel fügt jeder Protokollnachricht den Präfix "CUSTOM LOG: " hinzu und schließt das Datum, die Uhrzeit und den Quelldateiort ein.

## Tiefgehend

Das `log`-Paket der Go-Standardbibliothek ist unkompliziert und für viele Anwendungen ausreichend, aber es fehlen einige der ausgefeilteren Funktionen, die in Drittanbieter-Protokollierungsbibliotheken zu finden sind, wie strukturierte Protokollierung, Log-Rotation und protokollierung auf Basis von Ebenen. Pakete wie `zap` und `logrus` bieten diese fortgeschrittenen Funktionen und sind in der Go-Community für ihre Leistung und Flexibilität gut angesehen.

Die strukturierte Protokollierung ermöglicht es beispielsweise, Daten in einem strukturierten Format (wie JSON) zu protokollieren, was besonders nützlich für moderne, cloudbasierte Anwendungen ist, bei denen Protokolle von verschiedenen Werkzeugen oder Diensten analysiert werden könnten. `zap` ist insbesondere für seine hohe Leistungsfähigkeit und geringe Zuweisungsbelastung bekannt, was es für Anwendungen geeignet macht, bei denen Geschwindigkeit und Effizienz entscheidend sind.

Historisch gesehen hat sich die Protokollierung in Go seit der Einführung der Sprache erheblich weiterentwickelt. Frühe Versionen von Go boten die grundlegenden Protokollierungsfähigkeiten, die wir im `log`-Paket sehen. Doch als die Sprache an Popularität gewann und die Komplexität der in Go geschriebenen Anwendungen zunahm, begann die Gemeinschaft, ausgefeiltere Protokollierungsbibliotheken zu entwickeln, um ihre Bedürfnisse zu erfüllen. Heute, während das Standard-`log`-Paket für einfache Anwendungen weiterhin eine praktikable Option bleibt, wenden sich viele Entwickler für komplexere Protokollierungsanforderungen diesen Drittanbieterlösungen zu.
