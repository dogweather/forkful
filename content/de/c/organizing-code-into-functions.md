---
title:                "Code in Funktionen organisieren"
date:                  2024-01-26T01:08:59.137032-07:00
model:                 gpt-4-1106-preview
simple_title:         "Code in Funktionen organisieren"

category:             "C"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Was & Warum?
Code in Funktionen zu organisieren bedeutet, den Code in wiederverwendbare Blöcke zu unterteilen, die spezifische Aufgaben ausführen. Dies macht den Code leichter lesbar, zu debuggen und zu warten.

## Wie geht das:
Nehmen wir ein einfaches Beispiel: Nehmen wir an, Sie möchten mehrmals zwei Zahlen addieren.

Ohne Funktionen:
```C
#include <stdio.h>

int main() {
    int summe1 = 5 + 3;
    printf("Summe1: %d\n", summe1);
    
    int summe2 = 2 + 8;
    printf("Summe2: %d\n", summe2);
    
    // Weitere Additionen hier...
    
    return 0;
}
```

Mit Funktionen:
```C
#include <stdio.h>

int addiere(int a, int b) {
    return a + b;
}

int main() {
    int summe1 = addiere(5, 3);
    printf("Summe1: %d\n", summe1);
    
    int summe2 = addiere(2, 8);
    printf("Summe2: %d\n", summe2);
    
    // Verwenden Sie die Funktion addiere() für weitere Additionen...
    
    return 0;
}
```

Ausgabe:
```
Summe1: 8
Summe2: 10
```

## Vertiefender Einblick
Bevor es in C Funktionen gab, wurde Programmierung oft in linearer Weise durchgeführt, ähnlich wie ein Rezept. Aber als Programme größer wurden, wurde die Code-Duplizierung zu einem Problem. Funktionen waren die Lösung – sie ermöglichten es uns, denselben Codeblock von verschiedenen Teilen eines Programms aus auszuführen, ohne ihn jedes Mal neu schreiben zu müssen. Dadurch spart man nicht nur Platz, sondern auch Zeit bei Updates: Ändert man die Funktion an einem Ort, wird jeder Teil Ihres Codes, der sie verwendet, aktualisiert.

Alternativen zu Funktionen könnten Inline-Code, Makros oder Copy-Paste-Coding sein, aber diese können zu aufgeblähtem, fehleranfälligem und schwer zu wartendem Code führen. Funktionen hingegen kapseln Funktionalität, definieren klare Schnittstellen und können Nebenwirkungen durch die richtige Verwendung von Scope reduzieren.

Wenn Sie Funktionen implementieren, sollten Sie ein paar Details beachten: Erstens versuchen Sie, sie nur eine Sache tun zu lassen – dies ist als Single Responsibility Principle bekannt. Zweitens sind Namen wichtig – wählen Sie aussagekräftige Namen für Funktionen und deren Parameter, um Ihren Code selbstdokumentierend zu gestalten.

## Siehe auch
Für mehr über Funktionen in C, erkundigen Sie sich hier:

- C-Standardbibliothek Referenz: https://en.cppreference.com/w/c/header
- C Programmierung: Ein moderner Ansatz von K.N. King: Ein Buch mit einer detaillierten Betrachtung von Funktionen.
- Learn-C.org: Abschnitt über Funktionen: https://www.learn-c.org/de/Functions
