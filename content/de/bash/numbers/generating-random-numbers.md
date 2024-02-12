---
title:                "Generierung von Zufallszahlen"
aliases:
- de/bash/generating-random-numbers.md
date:                  2024-01-27T20:33:00.721556-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generierung von Zufallszahlen"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Generieren von Zufallszahlen in Bash bietet eine Möglichkeit, Unvorhersehbarkeit in Skripten einzuführen, was für Aufgaben wie das Generieren von sicheren Passwörtern, die Simulation von Daten oder für die Programmierung von Spielen unerlässlich ist. Programmierer nutzen diese Fähigkeit, um ihren Skripten Variabilität hinzuzufügen oder ihre Programme unter einer Vielzahl zufällig generierter Bedingungen zu testen.

## Wie:
In Bash ist die Variable `$RANDOM` die erste Anlaufstelle für die Generierung von Zufallszahlen. Jedes Mal, wenn Sie darauf Bezug nehmen, stellt Bash eine Pseudozufallszahl zwischen 0 und 32767 bereit. Lassen Sie uns einige praktische Beispiele erkunden:

```Bash
# Grundlegende Verwendung von $RANDOM
echo $RANDOM

# Generierung einer Zufallszahl in einem bestimmten Bereich (hier 0-99)
echo $(( RANDOM % 100 ))

# Generierung einer "sichereren" Zufallszahl, geeignet für Passwörter oder Schlüssel
# Verwendung von /dev/urandom mit dem od-Befehl
head -c 8 /dev/urandom | od -An -tu4

# Seeding von RANDOM für Reproduzierbarkeit
RANDOM=42; echo $RANDOM
```

Beispielausgabe (Anmerkung: Die tatsächliche Ausgabe variiert, da die Zahlen zufällig sind):
```Bash
16253
83
3581760565
17220
```

## Vertiefung
Der Mechanismus hinter Bashs `$RANDOM` erzeugt Pseudozufallszahlen, was bedeutet, dass sie einem Algorithmus folgen und theoretisch vorhersagbar sein können - ein potenzielles Sicherheitsrisiko für Anwendungen, die echte Unvorhersehbarkeit erfordern. Moderne kryptografische Anwendungen benötigen in der Regel Zufälligkeit, die von physischen Phänomenen oder von speziell dafür entworfenem Hardwarezubehör wie `/dev/urandom` oder `/dev/random` in Linux abgeleitet wird, welche Umgebungsrauschen sammeln.

Für gelegentliche oder nicht sicherheitskritische Aufgaben reicht `$RANDOM` aus und bietet den Vorteil der Einfachheit. Allerdings sollten Entwickler für kryptografische Zwecke oder wenn die Qualität der Zufälligkeit kritisch ist, nach anderen Werkzeugen und Sprachen Ausschau halten, die mit der Kryptografie im Sinn entworfen wurden, wie OpenSSL oder Programmiersprachen mit robusten Bibliotheken zur Generierung von Zufallszahlen.

Während Bashs `$RANDOM` seinen Zweck in Skripten erfüllt, die grundlegende Zufallszahlen benötigen, sollten seine Einschränkungen Entwickler dazu bewegen, sich für Anwendungen, bei denen die Qualität oder Sicherheit der Zufälligkeit wichtig ist, nach robusteren Lösungen umzusehen.
