---
title:                "Einen String großschreiben"
html_title:           "Fish Shell: Einen String großschreiben"
simple_title:         "Einen String großschreiben"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Grossschreibung einer Zeichenkette (String) bedeutet, jeden Buchstaben in Großbuchstaben zu ändern. Programmierer verwenden dies, um bestimmte Textelemente hervorzuheben oder standardisierte Formatierungen zu erzeugen.

## So geht's:

In Fish Shell verwenden Sie die `string upper`-Funktion, um einen String zu kapitalisieren. Anbei einige Beispiele:

```Fish Shell
set my_string "hallo welt"
set capitalized_string (string upper $my_string)
echo $capitalized_string
```

Die Ausgabe wäre: `HALLO WELT`.

## Vertiefung:

Historisch hat der Fish Shell Befehl `string upper` seinen Ursprung in klassischen Unix-Befehlen. Es gibt Alternativen dazu. Eine davon ist das Verwenden von `tr`:

```Fish Shell
echo "hallo welt" | tr '[:lower:]' '[:upper:]'
```

Die Ausgabe wäre ebenfalls: `HALLO WELT`.

Es ist allerdings wichtig zu beachten, dass die `tr`-Methode potenziell Kollateralschäden verursachen könnte, falls der Input Sonderzeichen enthält.

## Siehe Auch:

Für weitere Details, werfen Sie einen Blick auf die offizielle Fish Shell Dokumentation: https://fishshell.com/docs/current/cmds/string-upper.html

Besuchen Sie außerdem Stack Overflow, um verschiedene Verwendungsfälle und Diskussionen zu betrachten: https://stackoverflow.com/questions/tagged/fish