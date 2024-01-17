---
title:                "Reply with Die Aktuelle Datum Erhalten"
html_title:           "Go: Reply with Die Aktuelle Datum Erhalten"
simple_title:         "Reply with Die Aktuelle Datum Erhalten"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Abrufen des aktuellen Datums ist in der Programmierung ein häufiger Vorgang, der es ermöglicht, das aktuelle Datum und die Uhrzeit auf einem Computer zu ermitteln. Programmierer verwenden diese Funktion häufig in Anwendungen, um Benutzern genaue Zeitangaben zu liefern, in Datenbanken, um Einträge mit Zeitstempeln zu versehen oder in automatisierten Abläufen, um zeitabhängige Aufgaben auszuführen.

## Wie geht's?

Das Abrufen des aktuellen Datums in Go ist sehr einfach. Alles, was du brauchst, ist die Standardbibliothek "time" und die Funktion "Now()". Schau dir das folgende Beispiel an:

```
package main

import (
    "fmt"
    "time"
)

func main() {
    currentDate := time.Now()
    fmt.Println(currentDate)
}
```

Der obige Code importiert zunächst die "time" Bibliothek und verwendet dann die Funktion "Now()", um das aktuelle Datum und die Uhrzeit zu erhalten. Dieses Datum wird dann in der Variable "currentDate" gespeichert und mit der "Println()" Funktion ausgegeben.

Das Ergebnis des obigen Beispiels sieht beispielsweise so aus:
```
2020-12-08 13:35:10.6653698 +0100 CET m=+0.000889201
```

Du kannst auch das Format des Datums und der Uhrzeit anpassen, indem du die Funktionen "Format()" oder "strftime()" verwendest. Schau dir die offizielle Dokumentation für weitere Details an.

## Tiefer einblick

In der Vergangenheit war es nicht so einfach, das aktuelle Datum in einer Programmiersprache abzurufen. Frühere Methoden erforderten viel Code und waren fehleranfällig. Glücklicherweise ist das in Go nicht der Fall, da die Sprache von Anfang an entwickelt wurde, um häufige Aufgaben wie diese zu erleichtern.

Es gibt auch alternative Methoden, um das aktuelle Datum und die Uhrzeit abzurufen, wie z.B. die Verwendung von Bibliotheken von Drittanbietern oder das Aufrufen von Betriebssystemfunktionen direkt über die "syscall" Bibliothek. Diese Methoden können je nach Anwendungsfall nützlich sein, aber die eingebaute Funktion "Now()" ist in den meisten Fällen ausreichend.

Wenn du dich fragst, wie genau Go das Datum und die Uhrzeit abruft, nutzt die Funktion "Now()" die Systemaufrufe des Betriebssystems, um das aktuelle Datum und die Uhrzeit zu erhalten. Dies erfolgt über die "syscall" Bibliothek und kann bei Bedarf in deinen Code eingesehen werden.

## Siehe auch:

Offizielle Dokumentation der "time" Bibliothek: https://pkg.go.dev/time

Weitere Informationen zu "time" Formatierung und strftime: https://gobyexample.com/time-formatting-parsing