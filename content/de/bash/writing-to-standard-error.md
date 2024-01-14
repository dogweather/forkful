---
title:                "Bash: Schreiben auf Standardfehler"
simple_title:         "Schreiben auf Standardfehler"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum

Wer gerne mit Bash programmiert, wird wissen, dass ein wichtiger Bestandteil des Debugging-Prozesses das Schreiben von Fehlern in den Standard Error ist. In diesem Beitrag werden wir uns genauer damit auseinandersetzen, warum und wie man effektiv auf den Standard Error schreiben sollte.

## Wie

Um auf den Standard Error zu schreiben, gibt es in Bash verschiedene Möglichkeiten. Eine davon ist die Verwendung der `echo`-Funktion und die Weiterleitung des Outputs an den Standard Error mit dem `>&2`-Operator. Hier ist ein Beispielcode:

```
echo "Fehler: Benutzername nicht gefunden" >&2
```

Dieser Code schreibt den angegebenen Text direkt in den Standard Error, was hilfreich sein kann, um Fehlermeldungen zu generieren.

Man kann auch die `printf`-Funktion verwenden, um formatierte Texte in den Standard Error zu schreiben. Hier ist ein Beispielcode:

```
printf "Kritischer Fehler: %s ist nicht verfügbar\n" "$DATEI" >&2
```

Dieser Code schreibt den formatierten Text, der den Namen der fehlenden Datei enthält, in den Standard Error.

## Tieferer Einblick

Das Schreiben von Fehlern in den Standard Error ist wichtig, weil es eine klare Trennung zwischen Standard Output (das in der Regel für die Ausgabe von Ergebnissen verwendet wird) und Standard Error (das für Fehlermeldungen und Warnungen verwendet wird) ermöglicht. Dies erleichtert das Debugging und die Fehlerbehandlung, da die Fehlermeldungen getrennt von anderen Ausgaben angezeigt werden und somit leichter zu finden sind.

Man kann auch den Inhalt des Standard Error in eine Datei umleiten, um ihn später zu überprüfen oder zu analysieren. Dazu kann man den `2>`-Operator verwenden. Hier ein Beispielcode:

```
./mein_script.sh 2> fehlerprotokoll.txt
```

Dieser Code führt das Skript `mein_script.sh` aus und leitet alle Ausgaben des Standard Error in die Datei `fehlerprotokoll.txt`.

## Siehe auch

- [Bash-Dokumentation zu Standard Error](https://www.gnu.org/software/bash/manual/html_node/Redirections.html)