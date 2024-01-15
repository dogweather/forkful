---
title:                "Schreiben auf den Standardfehler"
html_title:           "Fish Shell: Schreiben auf den Standardfehler"
simple_title:         "Schreiben auf den Standardfehler"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum schreiben wir in die Standardfehlerausgabe? 

Wenn du in der Fish Shell arbeitest, könnte es vorkommen, dass du Programmierbefehle eingibst, die Fehler verursachen. Um diese Fehler zu erkennen und zu debuggen, ist es wichtig zu wissen, wie du sie in die Standardfehlerausgabe schreibst. In diesem Artikel werde ich erklären, warum wir in die Standardfehlerausgabe schreiben und wie du dies in der Fish Shell durchführen kannst.

## So geht's:

Um in die Standardfehlerausgabe zu schreiben, verwenden wir den Befehl `echo` mit dem Parameter `-e` und dem Redirect-Operator `2>`, der den Ausgabestrom in den Standardfehler umleitet. Hier ist ein Beispiel:

```Fish Shell
echo -e "Dies ist ein Fehler." 2>
```

Diese Zeile wird den Text "Dies ist ein Fehler." in die Standardfehlerausgabe schreiben. Du kannst auch Variablen in die Standardfehlerausgabe schreiben, indem du sie in geschweifte Klammern setzt, wie in diesem Beispiel:

```Fish Shell
set variable "Fehler"
echo -e "Es gab einen ${variable}." 2>
```

Die Ausgabe wird folgendermaßen aussehen:

```
Es gab einen Fehler.
```

## Tiefere Einblicke:

Wenn du tiefer in das Schreiben in die Standardfehlerausgabe eintauchen möchtest, gibt es einige weitere Dinge, die du beachten kannst. 

- Du kannst die Fehlermeldungen in Farbe formatieren, indem du den `red` oder `err` Farbcode verwendest. Zum Beispiel `echo -e (err "Fehlermeldung") 2>`.
- Du kannst auch "Pipefail" verwenden, um Fehler in Pipelines zu erkennen und in die Standardfehlerausgabe zu schreiben. Hier ist ein Beispiel: 

```Fish Shell
set -o pipefail
command1 | command2 2>
```

Dies wird Fehler in der Pipeline erkennen und in die Standardfehlerausgabe schreiben.

## Siehe auch:

- [Fish Shell Documentation - Standardfehlerausgabe schreiben](https://fishshell.com/docs/current/commands.html#echo)
- [Fish Shell Tutorial - Fehlerbehandlung](https://fishshell.com/docs/current/tutorial.html#error-handling)
- [Befehlsreferenz - Fish Shell Befehle](https://fishshell.com/docs/current/cmds.html)