---
title:                "Gleam: Die Verwendung von regulären Ausdrücken"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum

Möchtest du deine Programmieraufgaben schnell und effizient erledigen? Dann ist die Verwendung von regulären Ausdrücken in Gleam genau das Richtige für dich! Mit regulären Ausdrücken kannst du Muster in Texten suchen und ersetzen, was dir viel Zeit und Aufwand ersparen kann. Lass uns einen Blick darauf werfen, wie du reguläre Ausdrücke in Gleam verwenden kannst.

## Wie geht es?

Um reguläre Ausdrücke in Gleam zu nutzen, musst du zuerst das Modul `re` importieren. Dann kannst du die Funktion `matches` nutzen, um zu überprüfen, ob ein Muster in einem Text vorhanden ist. Schauen wir uns ein Beispiel an:

```Gleam
import re

let text = "Hallo Welt!"

if re.matches("\\w+", text) {
    // Das Muster "\\w+" sucht nach allen Wörtern im Text
    // Hier kannst du dann deine weiteren Aktionen ausführen
    io.println("Das Text enthält ein Wort.")
} else {
    io.println("Das Text enthält keine Wörter.")
}
```

Die Ausgabe wäre in diesem Beispiel:

```
Das Text enthält ein Wort.
```

Nun schauen wir uns ein Beispiel für die Verwendung von regulären Ausdrücken bei der Textersetzung an:

```Gleam
import re

let text = "Heute ist ein schöner Tag."

let new_text = re.replace("schön", "regnerisch", text)

io.println(new_text)
```

Die Ausgabe wäre in diesem Fall:

```
Heute ist ein regnerischer Tag.
```

## Tiefentauchen

Reguläre Ausdrücke bieten viele Möglichkeiten und können sehr nützlich sein. Hier sind einige weitere Tipps und Tricks für die Verwendung von regulären Ausdrücken in Gleam:

- Verwende `^` am Anfang eines Musters, um nur Texte zu matchen, die mit dem Muster beginnen
- Verwende `$` am Ende eines Musters, um nur Texte zu matchen, die mit dem Muster enden
- Verwende `*` um eine beliebige Anzahl an Zeichen zu matchen (inklusive keiner)
- Verwende `+` um mindestens ein Zeichen zu matchen
- Verwende `?` um ein optionales Zeichen zu matchen
- Verwende `.` um ein beliebiges Zeichen zu matchen
- Verwende `{n}` um genau n Zeichen zu matchen
- Verwende `{n,}` um mindestens n Zeichen zu matchen
- Verwende `{n,m}` um zwischen n und m Zeichen zu matchen

## Siehe auch

- [Gleam Dokumentation über reguläre Ausdrücke](https://gleam.run/book/features/regex.html)
- [Reguläre Ausdrücke Tutorial in Gleam](https://gleam.run/book/tutorials/regex.html)
- [Online Reguläre Ausdrücke Tester für Gleam](https://regex-gleam.netlify.app/)