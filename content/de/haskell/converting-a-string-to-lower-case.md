---
title:                "Haskell: Umwandlung eines Strings in Kleinbuchstaben"
simple_title:         "Umwandlung eines Strings in Kleinbuchstaben"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Die Konvertierung von Strings zu Kleinbuchstaben ist ein häufiges Problem in der Programmierung. Durch das Konvertieren von Strings zu Kleinbuchstaben können wir sicherstellen, dass unsere Eingaben einheitlich sind und wir mögliche Fehler vermeiden können.

## Wie man es macht

Das Konvertieren von Strings zu Kleinbuchstaben ist in Haskell einfach. Wir verwenden einfach die `map` Funktion, um über jeden Buchstaben im String zu iterieren und ihn in einen Kleinbuchstaben zu konvertieren.

```Haskell
-- Wir definieren eine Funktion, die einen String nimmt und ihn in Kleinbuchstaben konvertiert
toLower :: String -> String
-- Wir benutzen map, um über jeden Buchstaben zu iterieren und ihn mit `toLower` zu konvertieren
toLower str = map toLower str
```

Nun können wir unsere Funktion mit einem beliebigen String aufrufen und das Ergebnis sehen:

```Haskell
-- Beispiel: Konvertierung des Strings "HALLO" zu Kleinbuchstaben
toLower "HALLO"
-- Ausgabe: "hallo"
```

## Tiefer Einblick

Der Grund für die Benutzung von `map` bei der Konvertierung von Strings zu Kleinbuchstaben ist, dass es sich bei Strings in Haskell um Listen von Charakteren handelt. Daher ist es einfach, über jeden Buchstaben in einem String zu iterieren und ihn zu konvertieren.

Eine weitere Möglichkeit, Strings zu Kleinbuchstaben zu konvertieren, besteht darin, die `toLower` Funktion aus dem `Data.Char` Modul zu importieren und sie für jeden Charakter im String anzuwenden. Dies kann jedoch etwas umständlicher sein.

## Siehe auch

- [`map` Dokumentation in der Haskell-Dokumentation](https://www.haskell.org/hoogle/?hoogle=map)
- [Ein Tutorial für Haskell-Einsteiger](https://wiki.haskell.org/Haskell_in_5_minutes)
- [Offizielle Dokumentation für `Data.Char` Modul](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Char.html)