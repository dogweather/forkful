---
title:                "Suchen und Ersetzen von Text"
html_title:           "Bash: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Suche und Ersetze im Bash-Scripting

## Was und Warum?

Die Suche und das Ersetzen von Text sind grundlegende Programmierfähigkeiten. Damit manipulieren Entwickler schnell Textdaten, um nützliche Transformationen und Korrekturen durchzuführen.

## So geht's:

Bash verwendet das `sed` Befehlszeilenprogramm für die Textsuche und -ersetzung. Schauen wir uns ein kurzes Beispiel an.

```Bash
text = "Hallo, Welt!"
echo $text | sed s/Welt/Erde/
```

Das ergibt:

```Bash
Hallo, Erde!
```

## Tief tauchen:

Die `sed`-Befehlszeilentool wurde Anfang der 1970er Jahre erstellt und ist nach wie vor ein Standard-Textbearbeitungstool in Unix-artigen Systemen.

Alternativen zu `sed` sind `awk` und `perl`. Obwohl sie leistungsfähiger sind, erfordern sie eine intensivere Einarbeitung.

Bash selbst hat eingebaute Mechanismen zur Textmanipulation. Zum Beispiel kann der Befehl `${variable//suchen/ersetzen}` zum Ersetzen von Text in einer Variable verwendet werden.

```Bash
text="Hallo, Welt!"
echo ${text//Welt/Erde}
```

Das ergibt:

```Bash
Hallo, Erde!
```

## Siehe Auch:

Für weitere Informationen über die Bash-Programmierung und Textmanipulation, überprüfen Sie diese Ressourcen:

- [GNU sed Benutzerhandbuch](https://www.gnu.org/software/sed/manual/sed.html)
- [Advanced Bash-Scripting Guide](http://tldp.org/LDP/abs/html/)
- [Awk User's Guide](https://www.gnu.org/software/gawk/manual/gawk.html)