---
title:                "Einen Datum aus einem String parsen"
html_title:           "Elixir: Einen Datum aus einem String parsen"
simple_title:         "Einen Datum aus einem String parsen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Die Verarbeitung eines Datums aus einer Zeichenkette (String Parsing) ermöglicht es, aus Textdaten sinnvolle Datumswerte zu extrahieren und zu verwenden. Dies ist nützlich in Fällen, in denen Datums- und Zeitinformationen in Textform vorliegen und für Berechnungen, Sortierungen oder Filteroperationen verwendet werden müssen.

## So geht's:

Um ein Datum aus einem String zu lesen, können wir das `date` Befehl mit dem `-d` Schalter verwenden. 

```Bash
datum_string="2022-05-01 14:30"
datum=`date -d "$datum_string" +%Y-%m-%d_%H:%M`

echo "$datum"
```

Das obige Script wird folgendes ausgeben:

```Bash
2022-05-01_14:30
```

## Deep Dive

Historisch gesehen stammt das `date` Befehl aus den frühen Tagen von Unix (1970er Jahre). Alternativen sind das in Perl eingebaute `strftime` oder ähnliche Funktionen in anderen Programmiersprachen. Eine Herausforderung beim Parsing von Datumswerten aus Strings besteht darin, die vielen verschiedenen möglichen Formatierungen zu berücksichtigen.

## Siehe Auch

Für weitere Informationen zu diesem Thema, prüfen Sie bitte folgende Ressourcen:

- Bash `date` man page: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- Parsing dates in Bash: https://stackoverflow.com/questions/3919788/parsing-dates-in-bash
- Advanced Bash-Scripting Guide: http://tldp.org/LDP/abs/html/abs-guide.html