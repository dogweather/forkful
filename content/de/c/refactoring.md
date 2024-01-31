---
title:                "Refactoring"
date:                  2024-01-26T01:16:40.591328-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refactoring"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/refactoring.md"
---

{{< edit_this_page >}}

## Was & Warum?
Refactoring ist der Prozess der Umstrukturierung vorhandenen Computer-Codes, ohne dessen externes Verhalten zu ändern. Programmierer betreiben Refactoring, um die Lesbarkeit zu verbessern, die Komplexität zu reduzieren oder den Code wartbarer und skalierbarer zu machen, was langfristig eine Menge Zeit und Kopfschmerzen sparen kann.

## Wie:
Lassen Sie uns etwas Code aufpolieren. Nehmen wir an, Sie haben eine Funktion, die den Durchschnitt von Ganzzahlen in einem Array berechnet. Auf den ersten Blick ist es ein ziemliches Durcheinander.

**Vor dem Refactoring:**
```C
#include <stdio.h>

double calculateStuff(int numbers[], int size) {
    int sum = 0;
    double average;
    for(int i = 0; i < size; sum += numbers[i++]); // Summierung in der For-Schleifenbedingung, autsch!
    average = (double)sum / size;
    return average;
}

int main() {
    int array[] = {23, 42, 57, 16, 5, 102};
    int length = sizeof(array) / sizeof(array[0]);
    printf("Durchschnitt: %f\n", calculateStuff(array, length));

    return 0;
}
```

**Nach dem Refactoring:**
```C
#include <stdio.h>

int calculateSum(const int numbers[], int size) {
    int sum = 0;
    for (int i = 0; i < size; ++i) {
        sum += numbers[i];
    }
    return sum;
}

double calculateAverage(const int numbers[], int size) {
    int sum = calculateSum(numbers, size);
    return (double)sum / size;
}

int main() {
    int array[] = {23, 42, 57, 16, 5, 102};
    int length = sizeof(array) / sizeof(array[0]);
    printf("Durchschnitt: %f\n", calculateAverage(array, length));
    return 0;
}
```
Selbst mit diesem einfachen Beispiel sehen Sie, wie die Aufteilung der Funktion den Code sauberer und wartbarer macht. Jede Funktion hat jetzt eine einzige Verantwortung – ein Schlüsselprinzip im sauberen Programmieren.

## Vertiefung
Der Begriff "Refactoring" wurde Ende der 90er Jahre, insbesondere mit der Veröffentlichung von Martin Fowlers Buch "Refactoring: Improving the Design of Existing Code", populär. Refactoring impliziert nicht die Behebung von Bugs oder die Hinzufügung neuer Funktionen, sondern es geht um die Verbesserung der Struktur des Codes.

Es gibt viele ausgefeilte Refactoring-Tools und IDEs (Integrierte Entwicklungsumgebungen), die den Prozess automatisieren, wie CLion für C und C++, aber es bleibt entscheidend, zu verstehen, was unter der Haube vor sich geht.

Alternativen zum Refactoring können einschließen, Code von Grund auf neu zu schreiben (riskant und oft unnötig) oder mit der technischen Schuld zu leben (was langfristig kostspieliger sein kann). Umsetzungsdetails variieren je nach Projekt, aber gängige Refactorings umfassen das Umbenennen von Variablen zur Klarheit, das Aufbrechen großer Funktionen in kleinere und das Ersetzen von Magischen Zahlen durch benannte Konstanten.

Auch Muster wie DRY (Don't Repeat Yourself) und SOLID-Prinzipien können Ihren Refactoring-Weg leiten und für eine Codebasis sorgen, die einfacher zu testen, zu verstehen und an der gemeinsam gearbeitet werden kann.

## Siehe auch
Um tiefer in das Meer des Refactorings einzutauchen, werfen Sie einen Blick auf:

- Martin Fowlers Homepage: https://martinfowler.com/ mit einem Schatz an Artikeln und Ressourcen zum Thema Refactoring und Software-Design.
- Refactoring.com: https://refactoring.com/ bietet Beispiele und Kataloge von Refactoring-Techniken.
- Das Buch "Refactoring": Als Bibel des Refactorings betrachtet, bietet es eine komplette Sichtweise auf die Methodologie.
- "Clean Code: A Handbook of Agile Software Craftsmanship" von Robert C. Martin, das das Schreiben von Code, der leicht zu verstehen und zu warten ist, diskutiert.
