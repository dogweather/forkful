---
title:                "Generieren von Zufallszahlen"
html_title:           "PowerShell: Generieren von Zufallszahlen"
simple_title:         "Generieren von Zufallszahlen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?
Wenn du schon einmal eine Seite aktualisiert hast und jedes Mal ein zufälliges Bild haben wolltest, weißt du vielleicht schon, wie nützlich die Generierung von Zufallszahlen für Programmierer sein kann. Zufallszahlen sind eine unvorhersehbare Sequenz von Zahlen, die von Computern erzeugt werden und für verschiedene Zwecke verwendet werden können, wie z.B. für Spiele, Verschlüsselung oder Simulationen.

## Wie geht es?
Die Generierung von Zufallszahlen in PowerShell ist einfach und erfordert die Verwendung der integrierten Funktion "Get-Random". Hier ist ein Beispielcode, der eine zufällige Ganzzahl zwischen 1 und 100 ausgibt:

```
PowerShell Get-Random -Minimum 1 -Maximum 100
```
Die Ausgabe könnte z.B. "47" sein.

Um eine zufällige Dezimalzahl zu generieren, verwenden wir die Option "-InputObject", die in Kombination mit der Funktion "Get-Random" eine zufällige Zahl aus einer Liste von Werten auswählt. Hier ist ein Beispielcode, der eine zufällige Dezimalzahl aus einer Liste von 10, 20, 30, 40 und 50 auswählt:

```
PowerShell Get-Random -InputObject @(10, 20, 30, 40, 50)
```
Die Ausgabe könnte z.B. "30" sein.

## Tiefere Einblicke
In der Vergangenheit wurden Zufallszahlen durch physische Prozesse wie Würfeln oder Mischen von Karten erzeugt. Heutzutage werden jedoch Pseudozufallszahlen verwendet, die von Computern durch mathematische Algorithmen erstellt werden. Diese Algorithmen können nicht wirklich zufällige Zahlen erzeugen, aber ihre Ergebnisse sind ausreichend "zufällig" für die meisten Anwendungen.

Wenn du auf der Suche nach alternativen Methoden bist, um Zufallszahlen in PowerShell zu generieren, könntest du die Möglichkeit in Betracht ziehen, eine externe Bibliothek zu verwenden, die speziell für die Generierung von Zufallszahlen entwickelt wurde. Eine solche Bibliothek ist z.B. "Math::Random" aus dem Perl-Katalog CPAN.

Im Hinblick auf die Implementierung von Zufallszahlen in PowerShell gibt es einige interessante Details zu beachten. Die Version 5.1 von PowerShell verwendet die Random Number Generator-Schnittstelle (RNGCryptoServiceProvider), die sicherere Zufallszahlen in Anwendungen wie Verschlüsselung bietet. Ältere Versionen von PowerShell verwenden die Funktion "Rand()", die weniger sicher ist.

## Siehe auch
In diesem Artikel haben wir uns darauf konzentriert, wie man Zufallszahlen in PowerShell generiert. Wenn du mehr über die Verwendung von Zufallszahlen erfahren möchtest, empfehle ich dir, dich mit dem Thema "Pseudozufallszahlengeneratoren" zu beschäftigen. Hier ist ein Link zu einem interessanten Artikel für zusätzliche Informationen: https://de.wikipedia.org/wiki/Pseudozufallszahlengenerator