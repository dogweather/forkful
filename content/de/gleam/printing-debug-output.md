---
title:                "Gleam: Debug-Ausgabe drucken"
simple_title:         "Debug-Ausgabe drucken"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

# Warum
Erfahren Sie, warum das Drucken von Debug-Ausgaben beim Programmieren wichtig ist und wie es Ihnen helfen kann, Ihre Code-Probleme besser zu verstehen.

# Wie man das macht
Ein Beispiel dafür, wie man mit Gleam Debug-Ausgaben drucken kann:

```Gleam
let age = 25
debug::print("Das Alter ist:", age)
```

Die obige Zeile wird das folgende in der Konsole ausgeben:

```
Das Alter ist: 25
```

# Tiefer Einblick
Das Drucken von Debug-Ausgaben kann hilfreich sein, um den Ablauf eines Programms besser zu verstehen und mögliche Fehler aufzuspüren. Es hilft auch bei der Überprüfung von Variablenwerten und der Validierung von Berechnungen.

# Siehe auch
- [Debugging with Gleam](https://gleam.run/documentation/learn/tutorials/debugging.html)
- [Debug Your Code Like a Pro with Gleam](https://medium.com/hackernoon/debug-your-code-like-a-pro-with-gleam-51ebe8a35866)
- [Gleam Debug Library](https://github.com/gleam-lang/debug)

Vielen Dank fürs Lesen! Wir hoffen, dass Sie nun ein besseres Verständnis für das Drucken von Debug-Ausgaben in Gleam haben. Viel Spaß beim Debuggen!