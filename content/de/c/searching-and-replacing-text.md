---
title:                "Suchen und Ersetzen von Text"
aliases:
- de/c/searching-and-replacing-text.md
date:                  2024-02-03T18:08:17.276500-07:00
model:                 gpt-4-0125-preview
simple_title:         "Suchen und Ersetzen von Text"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/searching-and-replacing-text.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Das Suchen und Ersetzen von Text in C beinhaltet das Identifizieren spezifischer Teilstrings innerhalb eines größeren Strings und deren Substituierung durch verschiedene Teilstrings. Programmierer führen diese Operationen durch, um Textdaten zu manipulieren - für Aufgaben, die von der Datenbereinigung und Formatierung bis zur dynamischen Generierung von Inhalten reichen.

## Wie geht das:

C bietet keine integrierten Funktionen für das direkte Suchen und Ersetzen von Strings. Sie können dies jedoch erreichen, indem Sie verschiedene in der `<string.h>`-Bibliothek verfügbare String-Handling-Funktionen kombinieren zusammen mit einiger benutzerdefinierter Logik. Unten ist ein grundlegendes Beispiel dafür, wie man nach einem Teilstring innerhalb eines Strings sucht und ihn ersetzt. Der Einfachheit halber geht dieses Beispiel von ausreichender Puffergröße aus und behandelt keine Speicherzuweisungsprobleme, die Sie in Produktionscode berücksichtigen sollten.

```c
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void replaceSubstring(char *source, char *sub, char *new_sub) {
    char buffer[1024];
    char *insert_point = &buffer[0];
    const char *tmp = source;
    size_t len_sub = strlen(sub), len_new_sub = strlen(new_sub);
    size_t len_up_to_match;

    while ((tmp = strstr(tmp, sub))) {
        // Länge bis zur Übereinstimmung berechnen
        len_up_to_match = tmp - source;
        
        // Teil vor der Übereinstimmung kopieren
        memcpy(insert_point, source, len_up_to_match);
        insert_point += len_up_to_match;
        
        // Neuen Teilstring kopieren
        memcpy(insert_point, new_sub, len_new_sub);
        insert_point += len_new_sub;
        
        // Im Quellstring hinter der Übereinstimmung weitermachen
        tmp += len_sub;
        source = tmp;
    }
    
    // Verbleibenden Teil des Quellstrings kopieren
    strcpy(insert_point, source);
    
    // Modifizierten String ausgeben
    printf("Modifizierter String: %s\n", buffer);
}

int main() {
    char sourceStr[] = "Hallo, dies ist ein Test. Dieser Test ist einfach.";
    char sub[] = "Test";
    char newSub[] = "Beispiel";
    
    replaceSubstring(sourceStr, sub, newSub);
    
    return 0;
}
```

Beispielausgabe:
```
Modifizierter String: Hallo, dies ist ein Beispiel. Dieses Beispiel ist einfach.
```

Dieser Code demonstriert einen einfachen Ansatz, um nach allen Instanzen eines Teilstrings (`sub`) in einem Quellstring zu suchen und sie durch einen anderen Teilstring (`newSub`) zu ersetzen, wobei die Funktion `strstr` verwendet wird, um den Startpunkt jeder Übereinstimmung zu finden. Es ist ein sehr grundlegendes Beispiel, das komplexe Szenarien wie überlappende Teilstrings nicht behandelt.

## Tiefere Einblicke

Der Ansatz im Abschnitt "Wie geht das" ist grundlegend und veranschaulicht, wie man Textsuche und -ersatz in C ohne jegliche Drittanbieterbibliotheken erreichen kann. Historisch gesehen, aufgrund der Betonung von C auf Low-Level-Speicherverwaltung und Leistung, kapselt seine Standardbibliothek keine hochstufigen String-Manipulationsfunktionalitäten ein, wie sie in Sprachen wie Python oder JavaScript zu finden sind. Programmierer müssen den Speicher manuell verwalten und verschiedene String-Operationen kombinieren, um gewünschte Ergebnisse zu erzielen, was die Komplexität erhöht, aber mehr Kontrolle und Effizienz bietet.

Es ist wichtig zu beachten, dass dieser manuelle Ansatz fehleranfällig sein kann, insbesondere bei der Verwaltung von Speicherzuweisungen und Puffergrößen. Eine falsche Handhabung kann zu Pufferüberläufen und Speicherkorruption führen, was den Code für Sicherheitsrisiken anfällig macht.

In vielen praktischen Szenarien, insbesondere solchen, die komplexe Textverarbeitung erfordern, ist es oft erwägenswert, Drittanbieterbibliotheken wie PCRE (Perl Compatible Regular Expressions) für Regex-basierte Suche und Ersetzung zu integrieren, um den Code zu vereinfachen und das Potenzial für Fehler zu reduzieren. Zusätzlich bieten moderne C-Standards und Compiler zunehmend integrierte Funktionen und sicherere Alternativen für die String-Manipulation an, um gängige Fallstricke in älteren C-Codebasen zu mildern. Dennoch bleibt das grundlegende Verständnis der manuellen Textverarbeitung eine wertvolle Fähigkeit im Werkzeugkasten eines Programmierers, insbesondere für die Optimierung leistungskritischer Anwendungen.
