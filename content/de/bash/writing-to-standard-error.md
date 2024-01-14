---
title:                "Bash: Schreiben in die Standardfehlerausgabe"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum

Beim Programmieren mit Bash kann es immer mal wieder vorkommen, dass man auf Fehler stößt. Doch wie kann man diese Fehler effektiv verfolgen und beheben? Genau dafür ist es wichtig zu wissen, wie man in Bash zu Standard Error schreiben kann.

## Wie geht das?

Das Schreiben zu Standard Error in Bash ist ganz einfach. Man verwendet dafür einfach den Befehl `>&2`, gefolgt von dem Text oder der Fehlermeldung, die man ausgeben möchte. Ein Beispiel sieht wie folgt aus:

```Bash
echo "Dies ist eine Fehlermeldung" >&2
```

Dieser Befehl wird den Text "Dies ist eine Fehlermeldung" direkt zu Standard Error schreiben.

## Tieferes Eintauchen

Standard Error ist ein wichtiger Teil des Programmierens in Bash, da es hilft, Fehler schnell zu erkennen und zu beheben. Wussten Sie, dass Standard Error auch dazu verwendet werden kann, um unterschiedliche Ausgabefarben in der Terminal zu setzen? Hier ist ein Beispiel:

```Bash
echo -e "\033[31mDies ist eine rote Fehlermeldung\033[0m" >&2
```

Dieser Befehl wird den Text "Dies ist eine rote Fehlermeldung" in roter Farbe zu Standard Error schreiben. Die Farbcodes können je nach Bedarf angepasst werden.

## Siehe auch

- Bash-Scripting für Anfänger: https://www.linuxcommand.org/lc3_wss0010.php
- Fehlerbehandlung in Bash: https://wiki.bash-hackers.org/syntax/shellvars#special_variables
- Farben in Bash-Terminal setzen: https://misc.flogisoft.com/bash/tip_colors_and_formatting

Ich hoffe, dieser Artikel hat Ihnen geholfen, das Schreiben zu Standard Error in Bash besser zu verstehen. Happy Coding!