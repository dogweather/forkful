---
title:                "Zwei Daten vergleichen"
html_title:           "Swift: Zwei Daten vergleichen"
simple_title:         "Zwei Daten vergleichen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Vergleichen von zwei Daten ist eine häufige Aufgabe in der Programmierung. Wenn wir zwei Ereignisse oder Zeitpunkte haben, möchten wir oft wissen, welches früher oder später ist. Programmierer verwenden diese Technik, um Code zu schreiben, der auf bestimmte Zeitabläufe reagieren kann und somit die Funktionalität ihrer Programme zu verbessern.

## Wie?
Das Vergleichen von zwei Daten in Swift ist einfach und unkompliziert. Zuerst müssen wir zwei `Date`-Objekte haben, die die zu vergleichenden Daten repräsentieren. Dann verwenden wir die Vergleichsoperatoren wie `<` oder `>` um festzustellen, welches Datum früher oder später ist. Schauen wir uns ein Beispiel an:
```Swift
let date1 = Date()
let date2 = Date(timeIntervalSinceNow: 86400) // 1 Tag später
if date1 < date2 {
  print("date1 ist früher als date2")
} else {
  print("date2 ist früher als date1")
}
// Output: date2 ist früher als date1
```

## Tiefere Einblicke
Das Vergleichen von zwei Daten ist eine grundlegende Funktion in der Programmierung und wird in verschiedenen Situationen verwendet, wie z.B. bei Terminplanungen oder Ablaufsteuerungen. Neben den Vergleichsoperatoren können wir auch die Funktion `compare` verwenden, die uns eine `ComparisonResult`-Enumeration zurückgibt, die Auskunft darüber gibt, welches Datum früher oder später ist.

Es gibt auch Alternativen zur Verwendung von `Date`-Objekten, wie z.B. der`TimeInterval`-Datentyp, der auf Sekunden basiert und somit das Vergleichen von Zeitspannen ermöglicht. Die Implementierung des Datenvergleichs ist in jeder Programmiersprache ähnlich, aber die Unterschiede liegen in der Handhabung von Zeitformaten und Zeitbereichen.

Wenn du mehr über das Vergleichen von Daten in Swift erfahren möchtest, empfehlen wir dir die offizielle Dokumentation und das Swift Forum.

## Siehe auch
Offizielle Swift Dokumentation: https://docs.swift.org/swift-book/LanguageGuide/BasicOperators.html#ID70
Swift Forum: https://forums.swift.org/t/comparing-dates/21545