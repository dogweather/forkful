---
title:                "Strings verketten"
html_title:           "Bash: Strings verketten"
simple_title:         "Strings verketten"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Verkettung von Zeichenketten (String Concatenation) ist das Anhängen eines Strings an einen anderen. Programmierer machen das oft, um Anzeigen zu erstellen oder Informationen zu organisieren.

## Wie geht's?
In Clojure gibt es mehrere Möglichkeiten, Strings zu verketten. Hier sind einige Beispiele:

Grundlegende Verkettung mit `str`:

```Clojure
(str "Hallo" ", " "wie" " " "geht's?")
```
Dies gibt `"Hallo, wie geht's?"` aus.

Verkettung mit `format` für mehr Kontrolle:

```Clojure
(format "Hallo, %s!" "Welt")
```
Dies gibt `"Hallo, Welt!"` aus.

Verkettung von einer Liste von Strings:

```Clojure
(apply str ["Ich" " " "liebe" " " "Clojure!"])
```
Dies gibt `"Ich liebe Clojure!"` aus.

## Tiefgreifende Information

Historisch gesehen haben sich die Methoden zur Verkettung von Zeichenketten über die Zeit verändert, basierend auf Verbesserungen in Speicher und Performanz. In frühen Sprachen wie C gab es keine eingebaute Unterstützung für Zeichenkettenverkettung, was zu manuell verwalteten Puffern und schmerzhaften Buffer-Overflows führte.

Alternativen zur Verkettung von Strings in Clojure können die Nutzung von `clojure.string/join` oder `clojure.string/replace` sein, je nach Kontext.

Hinsichtlich der Implementierungsdetails arbeitet `str` in Clojure durch Erstellung einer einzelnen Zeichenkette aus den gegebenen Eingängen, während `format` String-Interpolation nutzt, um eine formatierte Zeichenkette zu erzeugen.

## Siehe auch

Nützliche Ressourcen zur weiteren Vertiefung:
- [Clojure: str](https://clojuredocs.org/clojure.core/str)
- [Clojure: format](https://clojuredocs.org/clojure.core/format)
- [Clojure: clojure.string/join](https://clojuredocs.org/clojure.string/join)
- [Clojure: clojure.string/replace](https://clojuredocs.org/clojure.string/replace) 

## Schlusswort

Es gibt keinen Abschlussabschnitt, aber das Erlernen von Clojure und dem Verketten von Zeichenketten kann ein wichtiger Schritt sein, um effizienterer und effektiver Programmierer zu werden. Happy Coding!