---
title:                "Schreiben auf Standardfehler"
aliases:
- de/python/writing-to-standard-error.md
date:                  2024-02-03T19:34:10.778212-07:00
model:                 gpt-4-0125-preview
simple_title:         "Schreiben auf Standardfehler"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?
Das Schreiben auf Standardfehler in Python bedeutet, die Fehlermeldungen oder Diagnosen Ihres Programms auf den Fehlerstrom (`stderr`) zu lenken, getrennt von der Standardausgabe (`stdout`). Programmierer tun dies, um normale Programmausgaben von Fehlermeldungen zu unterscheiden, was das Debuggen und die Log-Analyse erleichtert.

## Wie geht das:
### Verwendung von `sys.stderr`
Das in Python integrierte `sys`-Modul ermöglicht ein explizites Schreiben auf `stderr`. Dieser Ansatz ist unkompliziert für einfache Fehlermeldungen oder Diagnosen.

```python
import sys

sys.stderr.write('Fehler: Etwas ist schiefgelaufen.\n')
```
Beispielausgabe (auf stderr):
```
Fehler: Etwas ist schiefgelaufen.
```

### Verwendung der `print`-Funktion
Die `print`-Funktion von Python kann ihre Ausgabe umleiten auf `stderr`, indem der `file`-Parameter angegeben wird. Diese Methode ist nützlich, um die Benutzerfreundlichkeit von `print` bei der Behandlung von Fehlermeldungen zu nutzen.
```python
from sys import stderr

print('Fehler: Fehler im Modul.', file=stderr)
```
Beispielausgabe (auf stderr):
```
Fehler: Fehler im Modul.
```

### Verwendung des `logging`-Moduls
Für eine umfassendere Lösung kann das `logging`-Modul von Python Nachrichten auf `stderr` und vieles mehr lenken, wie das Schreiben in eine Datei oder das Anpassen des Nachrichtenformats. Diese Methode ist am besten für Anwendungen geeignet, die unterschiedliche Ebenen von Protokollierung, Nachrichtenformatierung oder Ziele erfordern.
```python
import logging

logging.basicConfig(level=logging.WARNING)
logger = logging.getLogger(__name__)

logger.error('Fehler: Datenbankverbindung fehlgeschlagen.')
```
Beispielausgabe (auf stderr):
```
FEHLER:__main__:Fehler: Datenbankverbindung fehlgeschlagen.
```

### Drittanbieter-Bibliotheken: `loguru`
`loguru` ist eine beliebte Drittanbieter-Bibliothek, die das Protokollieren in Python-Anwendungen vereinfacht. Fehler werden automatisch auf `stderr` geleitet, unter anderem.

Um `loguru` zu verwenden, installieren Sie es zunächst über pip:
```shell
pip install loguru
```

Dann binden Sie es wie folgt in Ihr Python-Skript ein:
```python
from loguru import logger

logger.error('Fehler: Datei konnte nicht geöffnet werden.')
```
Beispielausgabe (auf stderr):
```
2023-04-05 12:00:00.000 | FEHLER    | __main__:<module>:6 - Fehler: Datei konnte nicht geöffnet werden.
```
