---
aliases:
- /de/python/starting-a-new-project/
date: 2024-01-20 18:04:08.196793-07:00
description: "Ein neues Projekt zu starten bedeutet, eine frische Codebasis anzulegen,\
  \ um eine Idee in ein lauff\xE4higes Programm zu verwandeln. Programmierer machen\
  \ das,\u2026"
lastmod: 2024-02-18 23:09:04.462766
model: gpt-4-1106-preview
summary: "Ein neues Projekt zu starten bedeutet, eine frische Codebasis anzulegen,\
  \ um eine Idee in ein lauff\xE4higes Programm zu verwandeln. Programmierer machen\
  \ das,\u2026"
title: Einen neuen Projekt starten
---

{{< edit_this_page >}}

## Was & Warum?
Ein neues Projekt zu starten bedeutet, eine frische Codebasis anzulegen, um eine Idee in ein lauffähiges Programm zu verwandeln. Programmierer machen das, um Probleme zu lösen, Neues zu schaffen oder einfach zum Lernen.

## How to:
Um ein neues Projekt in Python zu beginnen, brauchst du als Erstes eine ordentliche Umgebung:

```Python
# Stelle sicher, dass Python installiert ist:
python --version

# Lege ein neues Verzeichnis für dein Projekt an:
mkdir mein_neues_projekt
cd mein_neues_projekt

# Initialisiere ein virtuelles Environment (optional, aber empfohlen)
python -m venv venv
source venv/bin/activate

# Installiere benötigte Pakete
pip install <paketname>
```

Sample output:

```
Python 3.10.2
```

Füge jetzt eine `main.py` Datei hinzu und beginne zu coden:

```Python
# main.py
print("Hallo Welt, ich bin ein neues Projekt!")
```

Führe es aus:

```bash
python main.py
```

Sample output:

```
Hallo Welt, ich bin ein neues Projekt!
```

## Deep Dive
Bevor wir so einfach Projekte starten konnten, mussten Entwickler ihre Entwicklungs- und Laufzeitumgebungen mühsam von Hand konfigurieren. Virtuelle Umgebungen, wie sie heute verfügbar sind, waren ein Game-Changer. Sie ermöglichen es, abhängige Softwarepakete projektweise zu isolieren. Alternativen zu `venv` sind `pipenv` oder `conda`.

Gut strukturierte Projekte verwenden oft `requirements.txt` für Paketabhängigkeiten oder `Pipfile` bei der Verwendung von `pipenv`. Für größere Projekte ist es wichtig, die Dateistruktur sauber zu halten. Module, Tests, Dokumentationen und Setup-Dateien sollten klar getrennt sein.

Ein weiterer wichtiger Aspekt ist die Versionierung deiner Software, besonders bei Zusammenarbeit im Team. Dazu werden oft Tools wie Git verwendet. Für die Verwaltung deines Codes stellen Plattformen wie GitHub, GitLab oder Bitbucket Repositories zur Verfügung.

## See Also
- Die offizielle Python-Dokumentation: https://docs.python.org/3/tutorial/venv.html
- Git-Einführung: https://git-scm.com/book/de/v2
- Pipenv: https://pipenv.pypa.io/en/latest/
- Projektstruktur-Leitfaden: https://docs.python-guide.org/writing/structure/
