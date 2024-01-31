---
title:                "Protokollierung"
date:                  2024-01-26T01:07:41.332387-07:00
model:                 gpt-4-1106-preview
simple_title:         "Protokollierung"

category:             "Go"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/logging.md"
---

{{< edit_this_page >}}

## Was & Warum?
Logging dreht sich um das Aufzeichnen von Ereignissen, Zuständen und Datenflüssen innerhalb einer App. Programmierer machen das, um Fehler zu diagnostizieren, die Leistung zu überwachen und den Betriebszustand der App zu verfolgen – es ist in etwa das Softwareäquivalent einer Black Box in Flugzeugen.

## Wie geht das:
In Go kann das Logging auf verschiedene Weisen gehandhabt werden, von dem `log`-Paket der Standardbibliothek bis hin zu Drittanbieter-Bibliotheken wie `logrus` und `zap`. Hier ist ein einfaches Beispiel unter Verwendung des eingebauten `log`-Pakets:

```Go
package main

import (
	"log"
	"os"
)

func main() {
	// Eine Log-Datei erstellen
	logFile, err := os.OpenFile("app.log", os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0666)
	if err != nil {
		log.Fatal(err)
	}
	defer logFile.Close()

	// Setze die Ausgabe des Logs auf die Datei
	log.SetOutput(logFile)

	// Einige Ereignisse loggen
	log.Println("Die Anwendung startet...")
	// ... Anwendungslogik hier ...
	log.Println("Die Anwendung wurde erfolgreich beendet.")
}
```

Wenn Sie diesen Code ausführen, sehen Sie keine Ausgabe im Terminal, weil alles in `app.log` geht. Hier ist ein Einblick, was Sie in der Log-Datei finden würden:

```
2023/01/02 15:04:05 Die Anwendung startet...
2023/01/02 15:05:01 Die Anwendung wurde erfolgreich beendet.
```

## Vertiefung
Das Logging in der Programmierung reicht zurück zu den frühesten Computern, wo Ingenieure buchstäblich Bugs (tatsächlich Motten) im Hardware gefangen und sie geloggt haben! Im Laufe der Zeit entwickelte sich das Logging zu einer raffinierten Methode, um zu verstehen, was in komplexen Systemen vor sich geht.

Obwohl das `log`-Paket in Go ziemlich simpel ist, kann es für grundlegende Anwendungen ausreichend sein. Jedoch, im Kontext moderner verteilter Systeme, oder wenn Sie eine nuanciertere Kontrolle über Ihre Log-Ausgabe benötigen (wie unterschiedliche Schweregrade), möchten Sie vielleicht robustere Lösungen erkunden.

Drittanbieter-Logging-Bibliotheken wie `logrus` und `zap` bieten strukturiertes Logging, was bedeutet, dass Sie komplexe Datentypen wie JSON loggen können, was die Interpretation von Logs erleichtert, besonders in Verbindung mit Log-Management-Systemen wie dem ELK Stack oder Splunk.

Bei der Überlegung der Implementierung einer Logging-Strategie ist es auch wesentlich, über die Leistungsimplikationen nachzudenken. Hochleistungs-Logging-Bibliotheken sind optimiert, um den Einfluss auf den Durchsatz und die Latenz der Anwendung zu verringern. Zum Beispiel rühmt sich `zap` mit seinem blitzschnellen, speicherschonenden Design, was für Echtzeitsysteme entscheidend sein kann.

Zusätzlich zu verschiedenen Bibliotheken sind auch Logging-Formate und Standards beachtenswert. Strukturierte Logging-Formate wie JSON können immens mächtig sein, wenn sie in Verbindung mit Log-Verarbeitungssystemen verwendet werden. Andererseits sind Klartext-Logs menschenlesbar, aber schwieriger programmatisch zu parsen.

## Siehe auch
Um tiefer in die Logging-Fähigkeiten von Go einzutauchen, könnten diese Ressourcen nützlich sein:

- Der Go-Blog zum Thema Logging: https://blog.golang.org/logging
- `logrus`, ein strukturierter Logger für Go: https://github.com/sirupsen/logrus
- `zap`, ein schneller, strukturierter, abgestufter Logger: https://github.com/uber-go/zap
- ELK Stack (Elasticsearch, Logstash, Kibana) für Log-Analyse: https://www.elastic.co/what-is/elk-stack
- Ein Vergleich von Go-Logging-Bibliotheken: https://www.loggly.com/blog/benchmarking-5-popular-golang-logging-libraries/
