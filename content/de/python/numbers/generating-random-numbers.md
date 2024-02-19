---
aliases:
- /de/python/generating-random-numbers/
date: 2024-01-27 20:35:04.950920-07:00
description: "Das Generieren von Zufallszahlen beinhaltet das Erstellen von Zahlen,\
  \ die nicht vern\xFCnftigerweise besser als durch Zufall vorhergesagt werden k\xF6\
  nnen, was\u2026"
lastmod: 2024-02-18 23:09:04.457730
model: gpt-4-0125-preview
summary: "Das Generieren von Zufallszahlen beinhaltet das Erstellen von Zahlen, die\
  \ nicht vern\xFCnftigerweise besser als durch Zufall vorhergesagt werden k\xF6nnen,\
  \ was\u2026"
title: Generierung von Zufallszahlen
---

{{< edit_this_page >}}

## Was & Warum?

Das Generieren von Zufallszahlen beinhaltet das Erstellen von Zahlen, die nicht vernünftigerweise besser als durch Zufall vorhergesagt werden können, was für die Entwicklung von Simulationen, Spielen und Sicherheitsalgorithmen unerlässlich ist. Programmierer tun dies, um Unvorhersehbarkeit einzuführen oder reale Phänomene in ihren Anwendungen zu simulieren.

## Wie:

Python bietet das `random`-Modul, das beim Generieren von Zufallszahlen für verschiedene Verwendungen hilft. So fängst du an:

1. **Importieren des Moduls**
    ```Python
    import random
    ```

2. **Generieren einer zufälligen Ganzzahl**
    Zwischen zwei beliebigen Zahlen.
    ```Python
    random_integer = random.randint(1, 10)
    print(random_integer)
    ```
    Beispiel-Ausgabe: `7`

3. **Generieren einer Fließkommazahl**
    Zwischen 0 und 1.
    ```Python
    random_float = random.random()
    print(random_float)
    ```
    Beispiel-Ausgabe: `0.436432634653`

    Wenn du eine Fließkommazahl in einem anderen Bereich benötigst, multipliziere:
    ```Python
    random_float_range = random.random() * 5  # 0 bis 5
    print(random_float_range)
    ```
    Beispiel-Ausgabe: `3.182093745`

4. **Auswählen eines zufälligen Elements aus einer Liste**
    ```Python
    greetings = ['Hallo', 'Hi', 'Hey', 'Hola', 'Bonjour']
    print(random.choice(greetings))
    ```
    Beispiel-Ausgabe: `Hola`

5. **Mischen einer Liste**
    Perfekt für Kartenspiele oder jede Anwendung, die eine zufällige Reihenfolge benötigt.
    ```Python
    numbers = list(range(10))
    random.shuffle(numbers)
    print(numbers)
    ```
    Beispiel-Ausgabe: `[2, 5, 0, 4, 9, 8, 1, 7, 6, 3]`

## Vertiefung

Das `random`-Modul in Python verwendet einen Pseudozufallszahlengenerator (PRNG), speziell den Mersenne-Twister-Algorithmus, der gut für allgemeine Anwendungen geeignet ist, aber aufgrund seiner Vorhersehbarkeit, wenn genügend Ausgaben beobachtet werden, nicht für kryptografische Zwecke geeignet ist. Das `secrets`-Modul, eingeführt in Python 3.6, bietet eine bessere Alternative für die Generierung kryptografisch starker Zufallszahlen, besonders nützlich bei sicherheitssensiblen Anwendungen. Zum Beispiel das Generieren eines sicheren, zufälligen Tokens für einen Passwort-Reset-Link:

```Python
import secrets
token = secrets.token_hex(16)
print(token)
```

Historisch gesehen war das Generieren von wirklich zufälligen Zahlen in der Informatik eine Herausforderung, wobei frühe Methoden auf physischen Phänomenen oder manuell eingegebenen Seeds beruhten. Die Entwicklung und Einführung von Algorithmen wie Mersenne Twister (standardmäßig im `random`-Modul von Python verwendet, zumindest bis zu meinem letzten Wissensstand im Jahr 2023) markierten bedeutende Fortschritte. Die fortlaufende Suche nach sichereren und effizienteren Algorithmen hat jedoch zur Aufnahme des `secrets`-Moduls für kryptografiebezogene Aufgaben geführt. Diese Evolution spiegelt die wachsende Bedeutung der Sicherheit in der Softwareentwicklung und den Bedarf an robusterem Zufall in Anwendungen von der Verschlüsselung bis zur sicheren Token-Generierung wider.
