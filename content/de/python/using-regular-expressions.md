---
title:                "Einsatz von regulären Ausdrücken"
date:                  2024-01-19
simple_title:         "Einsatz von regulären Ausdrücken"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Reguläre Ausdrücke, kurz Regex, sind Muster, um Text nach bestimmten Regeln zu durchsuchen und zu manipulieren. Programmierer verwenden sie, weil sie mächtig und effizient für Textanalyse und -verarbeitung sind.

## How to:

In Python verwenden wir das `re`-Modul, um mit Regulären Ausdrücken zu arbeiten. Hier sind ein paar Beispiele:

```Python
import re

# Beispiel: Überprüfe, ob eine Zeichenkette eine E-Mail-Adresse enthält
text = "schreib.mir@beispiel.de"
match = re.search(r"\b[\w\.-]+@[\w\.-]+\.\w{2,4}\b", text)
if match:
    print("Gefundene E-Mail-Adresse:", match.group())

# Beispiel: Trenne Text an jedem Komma
text = "Apfel, Birne, Banane, Kiwi"
gesplittet = re.split(r",\s*", text)
print("Aufgeteilte Wörter:", gesplittet)

# Beispiel: Ersetze alle Zahlen durch das Wort "Zahl"
text = "In diesem Jahr, 2021, wird Python 30."
ersetzt = re.sub(r"\d+", "Zahl", text)
print("Text mit ersetzen Zahlen:", ersetzt)
```

Erwartete Ausgabe:

```
Gefundene E-Mail-Adresse: schreib.mir@beispiel.de
Aufgeteilte Wörter: ['Apfel', 'Birne', 'Banane', 'Kiwi']
Text mit ersetzen Zahlen: In diesem Jahr, Zahl, wird Python Zahl.
```

## Deep Dive

Reguläre Ausdrücke haben ihre Wurzeln in der theoretischen Informatik, speziell in der Automatentheorie und der formalen Sprache. Sie wurden in den 1950er Jahren entwickelt und sind seitdem in verschiedenen Formen in Programmiersprachen und Texteditoren integriert worden.

Alternativen zu Regulären Ausdrücken sind Parser sowie textbasierte Such- und Ersetzungsfunktionen, die jedoch oft weniger mächtig sind.

Was das Arbeiten mit Regex in Python angeht, findet die Verarbeitung größtenteils im `re`-Modul statt. Dieses Modul verwendet eine Syntax, die sehr ähnlich zu anderen Programmiersprachen ist, wodurch die erlernten Muster oft portierbar sind.

## See Also

Weitere Informationen und tiefergehende Tutorials findest du in der offiziellen Python-Dokumentation zum `re`-Modul: https://docs.python.org/3/library/re.html

Für interaktives Üben empfehle ich Seiten wie https://regexr.com oder https://regex101.com, wo du Reguläre Ausdrücke testen und lernen kannst. 

Außerdem ist das Buch "Mastering Regular Expressions" von Jeffrey Friedl eine ausgezeichnete Ressource, um tiefer in das Thema einzutauchen.
