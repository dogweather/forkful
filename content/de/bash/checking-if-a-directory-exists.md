---
title:                "Überprüfen, ob ein Verzeichnis existiert"
html_title:           "Bash: Überprüfen, ob ein Verzeichnis existiert"
simple_title:         "Überprüfen, ob ein Verzeichnis existiert"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Bash Programmierung: Überprüfen ob ein Verzeichnis existiert

Willkommen. Heute werfen wir einen Blick auf das Szenario, in dem wir prüfen müssen, ob ein Verzeichnis in Bash existiert oder nicht. 

## Was & Warum?

Prüfen, ob ein Verzeichnis existiert, bedeutet herauszufinden, ob ein bestimmtes Verzeichnis in Ihrem Dateisystem vorhanden ist oder nicht. Programmierer tun dies, um Laufzeitfehler zu verhindern, die auftreten, wenn versucht wird, auf ein nicht existierendes Verzeichnis zuzugreifen.

## So geht's:

Im Bash-Skript verwenden wir die `-d` Option von Test, um zu überprüfen, ob ein Verzeichnis existiert oder nicht. Hier ist ein Code-Beispiel:

```Bash
if [ -d "$directory" ]; then
    echo "Das Verzeichnis existiert"
else
    echo "Das Verzeichnis existiert nicht"
fi
```

Ausgabe wäre:

```Bash
Das Verzeichnis existiert
oder
Das Verzeichnis existiert nicht
```

Je nachdem, ob das angegebene Verzeichnis vorhanden ist oder nicht.

## Tiefer eintauchen:

`-d` ist ein Unix-Bash-Symbol, das verwendet wird, um zu überprüfen, ob ein Dateisystem-Objekt ein Verzeichnis ist oder nicht. In der Anfangszeit von Unix war es die einzige Möglichkeit, dies zu tun. Alternativ könnten Sie in neueren Bash-Versionen `[[ -d "$directory" ]]` statt `[ -d "$directory" ]` verwenden. Dies stellt eine erweiterte Teststruktur dar, die eine detailliertere Fehlerprotokollierung ermöglicht. Intern verwendet Bash die `stat` Systemaufruf unter der Haube, um zu ermitteln, ob das Verzeichnis existiert oder nicht.

## Siehe auch:

- [Offizielle GNU Bash-Dokumentation](http://www.gnu.org/s/bash/)

- [Bash Beginners Guide](http://www.tldp.org/LDP/Bash-Beginners-Guide/html/)

*Diese Links führen Sie zu weiteren Ressourcen, wenn Sie mehr über Bash-Programmierung erfahren möchten. 

Bis zum nächsten Artikel!