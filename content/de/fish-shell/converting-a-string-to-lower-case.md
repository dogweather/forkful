---
title:                "Fish Shell: Umwandlung eines Strings in Kleinbuchstaben"
simple_title:         "Umwandlung eines Strings in Kleinbuchstaben"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Warum

In der Programmierung kommt es oft vor, dass wir Strings (Zeichenketten) in unseren Code einfügen müssen. Diese Strings können verschiedene Groß- und Kleinschreibung haben, was dazu führen kann, dass unser Code nicht richtig funktioniert. Um dieses Problem zu lösen, müssen wir Strings in Kleinbuchstaben umwandeln. In diesem Blogbeitrag werden wir sehen, wie man dies mit Fish Shell erreichen kann.

## Wie geht das?

Um einen String in Fish Shell in Kleinbuchstaben umzuwandeln, verwenden wir den Befehl `string tolower`. Dieser Befehl nimmt den angegebenen String und gibt ihn in Kleinbuchstaben zurück. Hier ist ein Beispiel:

```Fish Shell
string tolower "Hallo Welt"
```

Die Ausgabe dieses Befehls wäre `hallo welt`.

Wenn wir jedoch einen String mit mehr als einem Wort haben, sollte beachtet werden, dass der Befehl `string tolower` nur das erste Wort in Kleinbuchstaben umwandelt. Um alle Wörter in Kleinbuchstaben zu konvertieren, können wir den Befehl `string split` verwenden. Dieser Befehl teilt den String anhand eines Leerzeichens oder eines anderen angegebenen Trennzeichens auf und gibt eine Liste der einzelnen Wörter zurück. Wir können dann die `string tolower` auf jedes Element der Liste anwenden und am Ende die Liste wieder mit dem Befehl `string join` zu einem String zusammenfügen.

Hier ist ein Beispiel dafür, wie wir einen Wörterbuch-Eintrag in Kleinbuchstaben suchen und ausgeben können:

```Fish Shell
set woerterbuch "Hallo: Hello; Welt: World; Fisch: Fish"
set eingabe "fisch" # Eingabe des Benutzers
set eingabe_klein (string tolower $eingabe)

# Trennen des Wörterbuchs anhand eines Semikolons und Umwandeln in Liste
set uebersetzungen (string split ";" $woerterbuch)

for element in $uebersetzungen
  # Trennen des Elements anhand eines Doppelpunkts und Umwandeln in Liste
  set woerter (string split ":" $element)
  set deutsches_wort (string tolower $woerter[1])
  set englisches_wort (string tolower $woerter[2])

  if test "$eingabe_klein" = "$deutsches_wort"
    echo "Deutsche Übersetzung: $deutsches_wort"
    echo "Englische Übersetzung: $englisches_wort"
    break
  end
end
```

Die Ausgabe dieses Codes wäre:

```
Deutsche Übersetzung: fisch
Englische Übersetzung: fish
```

## Tiefer eintauchen

Beim Konvertieren von Strings in Kleinbuchstaben gibt es einige wichtige Dinge zu beachten. Zum Beispiel, dass dies nur für alphanumerische Zeichen funktioniert und keine Auswirkungen auf Sonderzeichen hat. Außerdem kann es je nach Shell-Umgebung zu Unterschieden in der Ausgabe kommen.

Wenn Sie mehr über die Interna von Fish Shell und die Umwandlung von Strings in Kleinbuchstaben erfahren möchten, können Sie die offizielle Dokumentation [hier](https://fishshell.com/docs/current/cmds/string.html#string-tolower) lesen.

# Siehe auch

- [Offizielle Dokumentation von Fish Shell](https://fishshell.com/docs/current/)
- [Umwandlung von Strings in Großbuchstaben](https://fishshell.com/docs/current/cmds/string.html#string-toupper) in Fish Shell
- [Weiterführende Artikel zu Fish Shell auf Medium](https://medium.com/search?q=fish%20shell)