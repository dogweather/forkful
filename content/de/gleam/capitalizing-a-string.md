---
title:                "Einen String großschreiben"
html_title:           "Gleam: Einen String großschreiben"
simple_title:         "Einen String großschreiben"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Großschreiben eines Strings ändert alle Anfangsbuchstaben der im String enthaltenen Worte in Großbuchstaben. Programmierer nutzen dieses Verfahren, um Texte hervorzuheben und optisch zu gliedern, besonders in Überschriften oder in Situationen, in denen Großschreibung einen Unterschied im Bedeutungskontext ausmacht.

## Wie geht's:

```Gleam
import gleam/string

fn main() {
  let mein_string = "hallo, welt!"
  let ergebnis = string.capitalize(mein_string)
  ergebnis
  |> should.equal(_ Ok("Hallo, Welt!") )
}
```
Ausgabe: 

`Hallo, Welt!`

## Tiefere Innenansichten:

(1) In der Vergangenheit gab es keine eingebaute Funktion, um Strings in Gleam zu capitalizen. Programmierer mussten den String in Zeichen aufteilen, das erste Zeichen manuell in einen Großbuchstaben umwandeln und den String wieder zusammenbauen.

(2) Es gibt Alternativen zur `string.capitalize` - Funktion, wie die Verwendung von Map oder Reduce - Funktionen, um über jeden Buchstaben im String zu iterieren und ihn je nach Bedarf zu ändern.

(3) Die Implementierung der `string.capitalize` - Funktion in Gleam erfolgt mithilfe der [case - Funktion](https://gleam.run/getting-started/references.html), die jeden Unicode-Buchstaben in einen Großbuchstaben umwandelt.

## Siehe auch:

Besuchen Sie die offizielle Gleam Dokumentation zur [string.capitalize - Funktion](https://gleam.run/getting-started/references.html) und [case - Funktion](https://gleam.run/getting-started/references.html), um mehr darüber zu erfahren, wie Sie Strings in Gleam manipulieren können. Ebenfalls hilfreich könnte der Leitfaden zum [Unicode-Handling in Gleam](https://gleam.run/notes/unicode.html) sein.