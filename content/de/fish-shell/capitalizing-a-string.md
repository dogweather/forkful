---
title:                "String in Großbuchstaben umwandeln"
date:                  2024-01-19
html_title:           "C: String in Großbuchstaben umwandeln"
simple_title:         "String in Großbuchstaben umwandeln"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Was & Warum?)
Großschreibung wandelt alle Buchstaben eines Strings in Großbuchstaben um. Diese Operation hilft bei der Vereinheitlichung von Eingabedaten oder bei der Benutzerinteraktion, indem sie z.B. Schlagworte in Texten hervorhebt.

## How to: (Wie macht man das?)
In Fish Shell ist das Kapitalisieren eines Strings simpel. Hier sind ein paar Code-Beispiele:

```Fish Shell
# Beispiel 1: Einen String in Großbuchstaben umwandeln
set my_string "wie geht's?"
echo $my_string | string to-upper

# Ausgabe: WIE GEHT'S?
```

```Fish Shell
# Beispiel 2: Kapitalisiere jeden Buchstaben in einer Liste von Strings
set my_list "hallo" "welt" "fish shell"
for word in $my_list
    echo $word | string to-upper
end

# Ausgabe:
# HALLO
# WELT
# FISH SHELL
```

## Deep Dive (Tiefere Einblicke)
Kapitalisierung in Programmiersprachen ist weit verbreitet und reicht bis in die frühen Tage der Computerei zurück. In Fish Shell wird der Kapitalisierungsbefehl `string to-upper` direkt unterstützt, was nicht in allen Shells der Fall ist. Alternativen in anderen Shells nutzen oft externe Programme wie `tr`, `awk`, oder `sed`.

Die Implementierung in Fish Shell ist effektiv und benutzerfreundlich, da `string` eine eingebaute Funktion der Shell ist und keine externen Aufrufe notwendig macht. Diese integrierte Funktion macht die Arbeit mit Strings schneller und reduziert die Fehleranfälligkeit im Vergleich zu mehrschrittigen Prozessen in anderen Shells.

## See Also (Weitere Quellen)
- [Fish Shell Documentation on String Manipulation](https://fishshell.com/docs/current/cmds/string.html)
- [Fish Shell Tutorial](https://fishshell.com/docs/current/tutorial.html)
- [GNU `awk` user's guide](https://www.gnu.org/software/gawk/manual/gawk.html)
