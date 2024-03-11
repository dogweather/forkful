---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:09.770581-07:00
description: "Das \xDCberpr\xFCfen, ob ein Verzeichnis in Python existiert, bedeutet,\
  \ die Anwesenheit eines Ordners im Dateisystem zu verifizieren, bevor Operationen\
  \ wie das\u2026"
lastmod: '2024-03-11T00:14:27.356701-06:00'
model: gpt-4-0125-preview
summary: "Das \xDCberpr\xFCfen, ob ein Verzeichnis in Python existiert, bedeutet,\
  \ die Anwesenheit eines Ordners im Dateisystem zu verifizieren, bevor Operationen\
  \ wie das\u2026"
title: "\xDCberpr\xFCfung, ob ein Verzeichnis existiert"
---

{{< edit_this_page >}}

## Was & Warum?
Das Überprüfen, ob ein Verzeichnis in Python existiert, bedeutet, die Anwesenheit eines Ordners im Dateisystem zu verifizieren, bevor Operationen wie das Lesen oder Schreiben von Dateien durchgeführt werden. Programmierer tun dies, um Fehler wie `FileNotFoundError` zu vermeiden und sicherzustellen, dass die Anwendung zuverlässig funktioniert und nicht abstürzt, wenn versucht wird, mit Verzeichnissen zu interagieren.

## Wie geht das:
Python bietet native Möglichkeiten, um die Existenz eines Verzeichnisses mit den Modulen `os` und `pathlib` zu überprüfen. Hier sind Beispiele für beide:

### Mit dem `os`-Modul
```python
import os

# Spezifizieren des Verzeichnispfades
dir_path = "/path/to/directory"

# Überprüfen, ob das Verzeichnis existiert
if os.path.isdir(dir_path):
    print(f"Das Verzeichnis {dir_path} existiert.")
else:
    print(f"Das Verzeichnis {dir_path} existiert nicht.")
```

### Mit dem `pathlib`-Modul
```python
from pathlib import Path

# Spezifizieren des Verzeichnispfades
dir_path = Path("/path/to/directory")

# Überprüfen, ob das Verzeichnis existiert
if dir_path.is_dir():
    print(f"Das Verzeichnis {dir_path} existiert.")
else:
    print(f"Das Verzeichnis {dir_path} existiert nicht.")
```

### Drittanbieter-Bibliotheken
Obwohl Pythons Standardbibliothek ausreicht, um zu überprüfen, ob ein Verzeichnis existiert, können Bibliotheken wie `pathlib2` Alternativen für Konsistenz über Python-Versionen hinweg oder für zusätzliche Funktionalitäten sein.

***Hinweis:*** In den neuesten Python-Versionen ist `pathlib` robust genug für die meisten Anwendungsfälle, was Drittanbieter-Bibliotheken für diese spezifische Aufgabe weniger notwendig macht.
