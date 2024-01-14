---
title:    "Swift: Berechnung eines Datums in der Zukunft oder Vergangenheit"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Das Berechnen eines Datums in der Vergangenheit oder Zukunft kann in vielen Situationen nützlich sein, zum Beispiel beim Erstellen von Erinnerungsfunktionen oder beim Planen von zukünftigen Ereignissen. Mit Swift können wir dies schnell und einfach erreichen, daher ist es wichtig zu verstehen, wie man es macht.

## Wie man es macht

In Swift gibt es mehrere Möglichkeiten, ein Datum in der Vergangenheit oder Zukunft zu berechnen. Eine einfache Möglichkeit ist die Verwendung der `Date` Klasse und ihrer Methoden `addingTimeInterval` oder `addingDateComponents`. Zum Beispiel, um 7 Tage in die Zukunft zu berechnen:

```Swift
let now = Date()
let future = now.addingTimeInterval(7 * 24 * 60 * 60) // 7 Tage entsprechen 7 * 24 * 60 * 60 Sekunden
```
Die `Date` Klasse nutzt Unix-Time, die die Anzahl der Sekunden seit dem 1. Januar 1970 angibt. Daher können wir einfach eine bestimmte Anzahl von Sekunden hinzufügen oder subtrahieren, um das gewünschte Datum zu erhalten.

Eine andere Möglichkeit ist die Verwendung von `Calendar` und `DateComponents`, um präziser zu berechnen, z.B. um einen Monat in die Zukunft zu gehen:

```Swift
let now = Date()
var dateComponents = DateComponents()
dateComponents.month = 1 // Eine Komponente für den Monat setzen
let future = Calendar.current.date(byAdding: dateComponents, to: now)
```

Beide Methoden liefern ein `Date` Objekt als Rückgabewert, das dann für weitere Berechnungen oder zum Anzeigen verwendet werden kann.

## Deep Dive

Beim Berechnen von Datumsangaben ist es wichtig, sich mit den verschiedenen Methoden und Klassen auseinanderzusetzen, um die gewünschten Ergebnisse zu erhalten. Die Verwendung von `Date` und `DateComponents` ermöglicht es uns, präzise Datumsangaben zu machen, während die Verwendung von Unix-Time in Kombination mit `Calendar` uns mehr Flexibilität bei der Berechnung von Zeitspannen gibt.

Es ist auch wichtig, sich mit der Darstellung von Datum und Zeit in verschiedenen Sprachen und Regionen auseinanderzusetzen, um sicherzustellen, dass die Berechnungen einheitlich und lesbar sind.

Da das Arbeiten mit Datum und Zeit ein wichtiger Teil der Programmierung ist, ist es ratsam, sich weiter mit dem Thema zu beschäftigen, um mögliche Fehler oder Probleme zu vermeiden.

## Siehe auch

- [Offizielle Swift Dokumentation](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID333)
- [NSHipster-Artikel über Datum und Zeit in Swift](https://nshipster.com/nsdateformatter/)
- [Hacking with Swift Tutorial über das Erstellen und Formatieren von Datum und Zeit](https://www.hackingwithswift.com/read/11/overview)