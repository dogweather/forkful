---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:58:04.012275-07:00
description: "Wie zu: C bietet keine integrierte Unterst\xFCtzung f\xFCr Ausnahmen\
  \ wie einige andere Sprachen. Stattdessen st\xFCtzt es sich auf einige konventionelle\u2026"
lastmod: '2024-03-13T22:44:54.360486-06:00'
model: gpt-4-0125-preview
summary: "C bietet keine integrierte Unterst\xFCtzung f\xFCr Ausnahmen wie einige\
  \ andere Sprachen."
title: Fehlerbehandlung
weight: 16
---

## Wie zu:
C bietet keine integrierte Unterstützung für Ausnahmen wie einige andere Sprachen. Stattdessen stützt es sich auf einige konventionelle Fehlerbehandlungsstrategien, wie das Zurückgeben spezieller Werte aus Funktionen und das Setzen globaler Variablen wie `errno`.

**Spezielle Werte zurückgeben**

Funktionen können Fehler anzeigen, indem sie einen spezifischen Wert zurückgeben, der unwahrscheinlich ein gültiges Ergebnis ist. Hier ist ein Beispiel mit Ganzzahlen:

```c
#include <stdio.h>

int inverse(int zahl, double *ergebnis) {
    if (zahl == 0) {
        return -1; // Fehlerfall
    } else {
        *ergebnis = 1.0 / zahl;
        return 0; // Erfolg
    }
}

int main() {
    double ergebnis;
    if (inverse(0, &ergebnis) < 0) {
        printf("Fehler: Division durch null.\n");
    } else {
        printf("Das Inverse ist: %f\n", ergebnis);
    }
    
    return 0;
}
```

**Ausgabe:**
```
Fehler: Division durch null.
```

**`errno` prüfen**

Für Bibliotheksfunktionen, insbesondere solche, die mit dem System oder dem Betriebssystem interagieren (wie Datei-I/O), wird `errno` gesetzt, wenn ein Fehler auftritt. Um es zu verwenden, inkludiere `errno.h` und prüfe `errno` nach einem vermuteten Fehler:

```c
#include <stdio.h>
#include <errno.h>
#include <string.h>

int main() {
    FILE *datei = fopen("nichtexistent.txt", "r");
    if (datei == NULL) {
        printf("Fehler beim Öffnen der Datei: %s\n", strerror(errno));
    }
    
    return 0;
}
```

**Ausgabe:**
```
Fehler beim Öffnen der Datei: Datei oder Verzeichnis nicht gefunden
```

## Tiefere Betrachtung
Historisch gesehen hat das minimalistische Design der C-Programmiersprache einen integrierten Ausnahmebehandlungsmechanismus ausgeschlossen, was seine Ursprünge in der Systemprogrammierung widerspiegelt, wo maximale Leistung und Kontrolle nahe an der Maschine kritisch sind. Stattdessen bevorzugt C einen manuelleren Ansatz zur Fehlerbehandlung, der zu seiner Philosophie passt, Programmierern so viel Kontrolle wie möglich zu geben, selbst auf Kosten der Bequemlichkeit.

Obwohl dieser Ansatz gut zu den Gestaltungszielen von C passt, kann er auch zu umfangreichem Fehlerprüfcode und der potenziellen Möglichkeit, Fehlerprüfungen zu verpassen, führen, was moderne Sprachen mit strukturierten Ausnahmehandlungsmechanismen ansprechen. Beispielsweise ermöglichen Ausnahmen in Sprachen wie Java oder C# eine zentralisierte Fehlerverarbeitung, die den Code sauberer und das Fehlermanagement einfacher macht. Allerdings führen Ausnahmen ihre eigenen Überkopfkosten und Komplexität ein, die für die Systemebenenprogrammierung, in der C glänzt, nicht ideal sein könnten.

Trotz seiner Grobheit hat diese manuelle Fehlerbehandlung in C das Design des Fehlermanagements in vielen anderen Sprachen geprägt und bietet ein Modell, bei dem die Explizitheit von Fehlerbedingungen zu vorhersehbarerem und leichter debuggbarem Code führen kann. Für kritische Systeme, bei denen Ausfälle elegant verwaltet werden müssen, stellt C's Fehlerbehandlungsparadigma - kombiniert mit modernen bewährten Methoden wie Fehlerbehandlungsbibliotheken und -konventionen - Robustheit und Zuverlässigkeit sicher.
