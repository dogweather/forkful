---
title:                "Umwandeln einer Zeichenfolge in Kleinbuchstaben"
html_title:           "Bash: Umwandeln einer Zeichenfolge in Kleinbuchstaben"
simple_title:         "Umwandeln einer Zeichenfolge in Kleinbuchstaben"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Wer regelmäßig mit Bash programmiert, wird sich früher oder später in der Situation befinden, eine eingegebene Zeichenfolge in Kleinbuchstaben zu konvertieren. Dies kann aus verschiedenen Gründen erforderlich sein, wie z.B. die Überprüfung von Benutzereingaben oder die Formatierung von Daten. In diesem Artikel werden wir sehen, wie man in Bash eine Zeichenfolge in Kleinbuchstaben umwandelt.

## Wie geht man vor

Um eine Zeichenfolge in Bash in Kleinbuchstaben umzuwandeln, gibt es verschiedene Ansätze. Hier sind zwei Beispiele:

```Bash
# Beispiel 1: Mit dem Befehl 'tr'
echo "HALLO WELT" | tr '[:upper:]' '[:lower:]'        # Ausgabe: hallo welt

# Beispiel 2: Mit dem von Bash unterstützten Parametererweiterung "${parameter,,}"
text="EINE ZEICHENFOLGE"
echo "${text,,}"                                      # Ausgabe: eine zeichenfolge
```

Die Verwendung des Befehls `tr` ist eine schnelle und einfache Möglichkeit, eine Zeichenfolge in Kleinbuchstaben umzuwandeln. Dieser Befehl ersetzt alle Großbuchstaben in der Zeichenfolge durch entsprechende Kleinbuchstaben. Auf der anderen Seite bietet die Parametererweiterung von Bash eine mehrsprachige Lösung, die auch Sonderzeichen korrekt verarbeitet. 

## Tiefer Einblick

Bei der Verwendung des Befehls `tr` ist es wichtig zu beachten, dass der Befehl die Zeichenfolge in der Standardeingabe erwartet. Dies bedeutet, dass sie entweder direkt über die Standardeingabe eingegeben oder durch einen Pipe-Befehl weitergeleitet werden muss. Alternativ können Sie den `tr` Befehl auch auf eine Variablengröße anwenden, wie im ersten Beispiel gezeigt.

Bei der Parametererweiterung von Bash ist es wichtig zu wissen, dass sie nicht von allen Bash-Versionen unterstützt wird. Sie wurde mit Bash 4 eingeführt und ist somit nicht auf älteren Versionen verfügbar. Daher sollte bei der Verwendung dieser Methode darauf geachtet werden, dass die Kompatibilität mit anderen Versionen sichergestellt ist.

## Siehe auch

- [Bash-Parametererweiterung](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
- [tr Befehl in der GNU Coreutils Doku](https://www.gnu.org/software/coreutils/manual/coreutils.html#tr-invocation)