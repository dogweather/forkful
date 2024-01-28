---
title:                "Refactoring"
date:                  2024-01-26T03:37:28.776764-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refactoring"
programming_language: "Python"
category:             "Python"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/refactoring.md"
---

{{< edit_this_page >}}

## Was & Warum?
Refactoring ist der Prozess des Umstrukturierens von bestehendem Computercode – die Änderung der Faktorisierung – ohne dessen externes Verhalten zu verändern. Programmierer tun dies, um den Code aufzuräumen, die Lesbarkeit zu verbessern und ihn einfacher zu warten und zu erweitern, alles ohne neue Funktionen hinzuzufügen.

## Wie:
Nehmen wir an, Sie haben einen Codeabschnitt, der die Fläche und den Umfang eines Rechtecks auf Basis seiner Länge und Breite berechnet und ausdruckt. Er erfüllt seinen Zweck, ist aber repetitiv und etwas unübersichtlich.

```python
# Ursprüngliche Version
length = 4
width = 3

# Berechnung von Fläche und Umfang
area = length * width
perimeter = 2 * (length + width)

print("Fläche:", area)
print("Umfang:", perimeter)
```

Wir können dies refaktorisieren, indem wir die Funktionalität in Funktionen kapseln, was den Code organisierter und wiederverwendbarer macht:

```python
# Refaktorisierte Version

def berechne_fläche(length, width):
    return length * width

def berechne_umfang(length, width):
    return 2 * (length + width)

# Nutzung
length = 4
width = 3

print("Fläche:", berechne_fläche(length, width))
print("Umfang:", berechne_umfang(length, width))
```

Beide Code-Schnipsel liefern das gleiche Ergebnis:
```
Fläche: 12
Umfang: 14
```

Aber die refaktorisierte Version ist sauberer und trennt die Anliegen, was es einfacher macht, eine Berechnung zu aktualisieren, ohne die andere zu beeinflussen.

## Tiefer Einstieg
Refactoring hat seine Wurzeln in den frühen Tagen des Software Engineerings, als Programmierer realisierten, dass Code verbessert werden könnte – und sollte –, selbst wenn er bereits "funktioniert". Martin Fowlers wegweisendes Buch "Refactoring: Improving the Design of Existing Code" formulierte viele Kernprinzipien und Techniken. Er sagte berühmt: "Jeder Dummkopf kann Code schreiben, den ein Computer verstehen kann. Gute Programmierer schreiben Code, den Menschen verstehen können."

Alternativen zum Refactoring könnten das Neuschreiben von Code von Grund auf oder das Durchführen kleiner Anpassungen ohne systematische Verbesserung umfassen. Allerdings ist Refactoring in der Regel kosteneffektiver als ein Neuschreiben und weniger riskant als Ad-hoc-Modifikationen. Implementierungsdetails können spezifisch für jedes Programmierparadigma sein; jedoch eignet sich die objektorientierte Programmierung besonders gut für das Refactoring, insbesondere mit Techniken wie dem Extrahieren von Methoden (wie unsere Funktionen `berechne_fläche` und `berechne_umfang`), Inline-Setzung, dem Verschieben von Features zwischen Objekten und dem Umbenennen von Methoden oder Variablen zur Klarheit.

Refactoring in Python verwendet häufig Werkzeuge wie `PyCharm`, das integrierte Refactoring-Fähigkeiten besitzt, oder `rope`, eine für das Refactoring speziell entwickelte Python-Bibliothek. Die sorgfältige Verwendung von Versionskontrolle, wie zum Beispiel `git`, während des Refactorings wird dringend empfohlen, um Änderungen schrittweise nachzuverfolgen.

## Siehe auch
Für diejenigen, die nach mehr suchen, tauchen Sie in die folgenden Ressourcen ein:
- Martin Fowlers Buch: [Refactoring: Improving the Design of Existing Code](http://www.refactoring.com/)
- Python-Refactoring mit `rope`: [GitHub - rope](https://github.com/python-rope/rope)
- PyCharm-Refactoring-Dokumentation: [Jetbrains PyCharm Refactoring Source Code](https://www.jetbrains.com/help/pycharm/refactoring-source-code.html)
- Refactoring.guru: [Refactoring und Design Patterns](https://refactoring.guru/refactoring)
- Clean Code-Vorträge von Uncle Bob (Robert C. Martin): [Clean Code - Uncle Bob / Lektion 1](https://www.youtube.com/watch?v=7EmboKQH8lM)
