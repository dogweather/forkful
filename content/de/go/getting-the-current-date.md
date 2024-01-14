---
title:    "Go: Das aktuelle Datum bekommen"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Warum

Das aktuelle Datum und die Uhrzeit sind eine wichtige Information, die in vielen Anwendungen verwendet wird. Zum Beispiel können Benutzer sehen, wann ein Blog-Beitrag veröffentlicht wurde oder wann ein Chat-Nachricht gesendet wurde. Es kann auch hilfreich sein, das Datum und die Uhrzeit zu protokollieren, um Aktionen und Ereignisse zu verfolgen. In diesem Beitrag zeigen wir Ihnen, wie Sie das aktuelle Datum in Go programmieren können.

## Wie man vorgeht

Um das aktuelle Datum in Go zu erhalten, können wir die built-in Funktion `time.Now()` verwenden. Diese Funktion gibt ein `time.Time` Objekt zurück, das Datum, Uhrzeit und Zeitzone enthält. Wir können dann Methoden anwenden, um das Datum und die Uhrzeit in verschiedenen Formaten auszugeben.

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    // das aktuelle Datum und Uhrzeit erhalten
    now := time.Now()

    // das Datum formatieren (Tag.Monat.Jahr)
    date := now.Format("02.01.2006")

    // die Uhrzeit formatieren (Stunden:Minuten:Sekunden)
    time := now.Format("15:04:05")

    // das Datum und die Uhrzeit ausgeben
    fmt.Printf("Heute ist der %s um %s Uhr", date, time)
}
```

Die Ausgabe würde folgendermaßen aussehen: `Heute ist der 05.01.2020 um 16:32:10 Uhr`. Wir können auch andere Formatierungsoptionen verwenden, um das Datum und die Uhrzeit auf unsere Bedürfnisse anzupassen.

## Tiefer gehende Informationen

Die `time.Time` Struktur enthält viele Methoden, die uns dabei helfen können, das Datum und die Uhrzeit in verschiedenen Zeitzonen, Formaten und Sprachen darzustellen. Wir können auch Berechnungen mit Datum und Uhrzeit durchführen, wie zum Beispiel die Dauer zwischen zwei Zeitpunkten oder das Hinzufügen von Stunden zu einem Datum.

Für eine detailliertere Erklärung der `time` Package in Go, empfehlen wir die offizielle Dokumentation: [https://golang.org/pkg/time/](https://golang.org/pkg/time/).

## Siehe auch
- [Einführung in die Programmierung mit Go](https://golang.org/doc/tutorial/)
- [Wie man Benutzereingaben in Go verarbeitet](https://www.programming-books.io/essential/go/4zuk-wie-liest-man-benutzereingaben-in-go-2a3f3c3af25a43d585a01c6a8e08c5cc)
- [Go: Die Grundlagen der Sprache](https://www.golang-book.com/books/intro)