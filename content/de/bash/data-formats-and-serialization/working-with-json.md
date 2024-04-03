---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:32.672077-07:00
description: "Mit JSON in Bash-Programmierung zu arbeiten bedeutet, JSON-Daten direkt\
  \ von der Kommandozeile aus zu parsen, zu extrahieren und zu manipulieren.\u2026"
lastmod: '2024-03-13T22:44:54.080999-06:00'
model: gpt-4-0125-preview
summary: Mit JSON in Bash-Programmierung zu arbeiten bedeutet, JSON-Daten direkt von
  der Kommandozeile aus zu parsen, zu extrahieren und zu manipulieren.
title: Arbeiten mit JSON
weight: 38
---

## Was & Warum?
Mit JSON in Bash-Programmierung zu arbeiten bedeutet, JSON-Daten direkt von der Kommandozeile aus zu parsen, zu extrahieren und zu manipulieren. Programmierer tun dies oft, um Shell-Skripte nahtlos mit Web-APIs und modernen Datenübertragungsformaten zu integrieren, was das Bash-Scripting in einem JSON-lastigen Ökosystem leistungsstärker und relevanter macht.

## Wie geht das:
Bash selbst hat keine eingebauten JSON-Parsing-Fähigkeiten, aber `jq` ist ein leistungsfähiger Kommandozeilen-JSON-Prozessor, der diese Lücke füllt. So nutzen Sie es:

**Ein JSON-File lesen:**

Beispiel `data.json`:
```json
{
  "name": "Jane Doe",
  "email": "jane@example.com",
  "location": {
    "city": "New York",
    "country": "USA"
  }
}
```

Um den Namen aus der JSON-Datei zu lesen und zu extrahieren:
```bash
jq '.name' data.json
```
Ausgabe:
```
"Jane Doe"
```

**JSON-Daten modifizieren:**

Um die Stadt auf "Los Angeles" zu aktualisieren und zurück in die Datei zu schreiben:
```bash
jq '.location.city = "Los Angeles"' data.json > temp.json && mv temp.json data.json
```

**JSON aus einer Variablen parsen:**

Wenn Sie JSON in einer Bash-Variablen haben, kann `jq` es trotzdem verarbeiten:
```bash
json_string='{"name": "John Doe", "email": "john@example.com"}'
echo $json_string | jq '.name'
```
Ausgabe:
```
"John Doe"
```

**Mit Arrays arbeiten:**

Gegeben sei ein Array von Elementen in JSON:
```json
{
  "items": ["apple", "banana", "cherry"]
}
```

Um das zweite Element zu extrahieren (Indizierung beginnt bei 0):
```bash
jq '.items[1]' data.json
```
Ausgabe:
```
"banana"
```

Für komplexere Operationen und Filterungen hat `jq` ein umfassendes Handbuch und Tutorials online verfügbar, was es zu einem vielseitigen Werkzeug für all Ihre Bash/JSON-Anforderungen macht.
