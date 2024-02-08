---
title:                "Zufallszahlen generieren"
aliases:
- de/vba/generating-random-numbers.md
date:                  2024-02-01T21:53:53.716924-07:00
model:                 gpt-4-0125-preview
simple_title:         "Zufallszahlen generieren"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/vba/generating-random-numbers.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Das Generieren von Zufallszahlen in Visual Basic for Applications (VBA) ermöglicht es Programmen, Prozesse mit Elementen des Zufalls oder der Variabilität zu simulieren, wie Würfelwürfe oder das Samplen von Daten. Programmierer verwenden diese Techniken, um Modelle, Spiele oder Simulationen zu entwickeln, bei denen vorhersehbare Ergebnisse unrealistisch oder weniger nützlich wären.

## Wie:

In VBA wird die `Rnd`-Funktion verwendet, um Zufallszahlen zu generieren. Standardmäßig erzeugt `Rnd` eine Fließkommazahl (Single-Precision) größer oder gleich 0 und kleiner als 1. Hier sind einige Schritte und Beispiele, um Zufallszahlen effektiv zu nutzen:

1. **Einfache Zufallszahl:**
   Um eine einfache Zufallszahl zu generieren, müssen Sie nur `Rnd()` aufrufen:

   ```vb
   Sub GenerateRandomNumber()
       Dim randomNumber As Single
       randomNumber = Rnd() ' Zufallszahl zwischen 0 und 1
       MsgBox randomNumber
   End Sub
   ```

2. **Den Seed setzen:**
   Die `Randomize`-Anweisung initialisiert den Zufallszahlengenerator, was entscheidend sein kann, um bei jedem Ausführen Ihres VBA-Codes unterschiedliche Ergebnisse zu gewährleisten:

   ```vb
   Sub SeedRandomNumber()
       Randomize
       Dim randomNumber As Single
       randomNumber = Rnd()
       MsgBox randomNumber
   End Sub
   ```

3. **Zahlen in einem Bereich generieren:**
   Oft möchten Sie eine Zufallszahl innerhalb eines bestimmten Bereichs haben. So erzeugen Sie eine Zahl zwischen 1 und 100:

   ```vb
   Sub RandomNumberInRange()
       Randomize
       Dim randomNumber As Integer
       randomNumber = Int((100 * Rnd()) + 1) ' Zufallszahl zwischen 1 und 100
       MsgBox randomNumber
   End Sub
   ```

### Beispiel-Ausgabe:
Nach dem Ausführen von `RandomNumberInRange` könnten Sie eine Nachrichtenbox sehen, die eine Zahl wie `45` anzeigt.

## Tiefergehend:

Die `Rnd`-Funktion in VBA, obwohl einfach zu verwenden, generiert eigentlich pseudo-zufällige Zahlen basierend auf einem deterministischen Algorithmus. Das bedeutet, dass die Sequenzen der Zahlen, die sie produziert, nicht wirklich zufällig sind, aber oft ausreichen für allgemeine Aufgaben, die stochastische Prozesse benötigen.

Historisch gesehen reicht die Fähigkeit zur Zufallszahlengenerierung in VBA zurück bis zu frühen Versionen von Basic, die sich im Laufe der Zeit weiterentwickelt haben, um Funktionen wie `Randomize` einzuschließen, um die Zufälligkeit zu verbessern, indem der Algorithmus mit einem Startpunkt gespeist wird. Jedoch, für Anwendungen, die hohe Niveaus an Zufälligkeit benötigen, wie sichere kryptografische Operationen, ist VBA’s `Rnd` möglicherweise nicht das beste Werkzeug. Alternativen in robusteren Programmierumgebungen oder Sprachen, die mit Kryptographie im Sinn entworfen wurden, wie das `secrets`-Modul in Python oder `SecureRandom` in Java, sollten in Betracht gezogen werden.

Trotz seiner Einschränkungen macht die Einfachheit und Zugänglichkeit der Generierung von Zufallszahlen in VBA es weiterhin zu einem wertvollen Werkzeug für eine breite Palette von leichteren Anwendungen, Simulationsarbeiten und Bildungszwecken.
