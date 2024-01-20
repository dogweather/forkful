---
title:                "Schreiben auf Standardfehler"
html_title:           "Arduino: Schreiben auf Standardfehler"
simple_title:         "Schreiben auf Standardfehler"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?

Schreiben auf Standardfehler (stderr) ermöglicht es Programmen, Fehlermeldungen von der normalen Ausgabe zu trennen. Das ist wichtig für die Fehlersuche und hilft dabei, die normale Ausgabe in Dateien umzuleiten, ohne Fehlermeldungen zu verlieren.

## How to:

### Fehlermeldungen nach stderr schreiben:
```Bash
echo "Das ist ein Fehler" >&2
```

### Normale Ausgabe und Fehlerausgabe umleiten:
```Bash
echo "Normale Ausgabe"
echo "Das ist ein Fehler" >&2
```
Sample Output:
```
Normale Ausgabe
Das ist ein Fehler
```
### Ausgabe in eine Datei und Fehlermeldungen in eine andere Datei umleiten:
```Bash
echo "Normale Ausgabe" > output.txt
echo "Das ist ein Fehler" >&2 2>error.txt
```

## Deep Dive

In den Anfangstagen von Unix wurden zwei separate Datenströme eingeführt: Standardausgabe (stdout) für die reguläre Ausgabe eines Programms und Standardfehler (stderr) für Fehlermeldungen. Das trennt Fehler von normalen Daten, was besonders wichtig ist bei Pipelines und Weiterleitungen. Alternativen wie das Schreiben beider Streams in eine Datei (`&> file.txt`) sind möglich, aber die Trennung ist oft sinnvoller. Bei der Implementierung leitet der File Descriptor 2 stderr, während File Descriptor 1 stdout anspricht.

## See Also

- Bash Manual: https://www.gnu.org/software/bash/manual/
- Advanced Bash-Scripting Guide: http://www.tldp.org/LDP/abs/html/
- Unix Streams, Pipes, and Redirects: https://www.ibm.com/support/knowledgecenter/ssw_ibm_i_74/rzabc/redirection.htm