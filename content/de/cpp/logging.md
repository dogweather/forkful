---
title:                "Protokollierung"
date:                  2024-01-26T00:59:57.140829-07:00
model:                 gpt-4-1106-preview
simple_title:         "Protokollierung"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/logging.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Protokollieren im Kontext der Programmierung ist der Prozess, Ereignisse, Zustände und Informationen in einer Datei oder einem anderen Ausgabemedium aufzuzeichnen. Programmierer führen Protokolle, um den Überblick darüber zu behalten, was in ihren Anwendungen passiert, um Probleme zu debuggen und um die Leistung für zukünftige Analysen und Optimierungen zu überwachen.

## Wie man das macht:
Angenommen, Sie arbeiten an einem Linux-Computer und möchten Ihre Protokolle mit gutem alten C++ in eine Datei schreiben. Sie sollten die Bibliotheken `<iostream>` und `<fstream>` einbinden, um Dateioperationen durchzuführen. Hier ist ein kurzes Beispiel:

```C++
#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ofstream logFile("appLog.txt", std::ios::app);  // Im Anhänge-Modus öffnen

    if (!logFile.is_open()) {
        std::cerr << "Es gab ein Problem beim Öffnen der Protokolldatei!" << std::endl;
        return 1;
    }

    logFile << "Anwendung gestartet" << std::endl;
  
    // ... irgendwo in Ihrer Anwendungslogik
    logFile << "Es ist ein wichtiges Ereignis eingetreten" << std::endl;

    // Vergessen Sie nicht, Ihren Dateistream zu schließen
    logFile.close();

    return 0;
}
```

Wenn Sie Ihre Protokolldatei mit `tail -f appLog.txt` überwachen, sollten Sie Folgendes sehen:

```
Anwendung gestartet
Es ist ein wichtiges Ereignis eingetreten
```

Toll, Sie haben jetzt einen zeitgestempelten Verlauf von Ereignissen!

## Vertiefung
Das Protokollieren ist so alt wie die Datenverarbeitung selbst, mit Wurzeln in buchstäblichen Markierungen auf Papier, um nachzuverfolgen, was alte Computer taten. In der modernen Ära geht es um ausgeklügelte Softwarelösungen. Es gibt direkte Dateiprotokollierung, wie das schnelle und schmutzige Beispiel oben, oder Sie könnten sich für ein ausgefeilteres Protokollierungs-Framework entscheiden, wie Log4cpp oder Boost.Log im C++-Bereich; diese bieten Protokollierungsstufen, Formatkontrolle und mehr.

Apropos Stufen, zu den besten Praktiken beim Protokollieren gehört die Verwendung unterschiedlicher Schweregrade – Info, Debug, Warnung, Fehler, Fatal –, sodass Sie das Rauschen filtern können, wenn Sie versuchen, Bugs zu beheben oder herauszufinden, warum sich Ihre App wie ein launischer Teenager verhält.

In Bezug auf die Leistung sollten Sie nicht nachlässig mit Ihren Protokollen werden. Übermäßiges Protokollieren kann aus Ihrer blitzschnellen App einen Schneckenmarathon machen, Dateisysteme belasten oder sogar Kosten in Speichergebühren verursachen, wenn Sie cloudbasiert sind. Das richtige Gleichgewicht zu finden, ist der Schlüssel: Protokollieren Sie das, was Sie benötigen, und nichts mehr.

## Siehe Auch
Für diejenigen unter Ihnen, die bei Ihren Protokollierungspraktiken die Extrameile gehen wollen, sehen Sie sich diese an:

- Die [Boost.Log-Bibliothek](https://www.boost.org/doc/libs/1_75_0/libs/log/doc/html/index.html) für einige hochleistungsfähige Protokollierungsfunktionen.
- [Googles glog-Bibliothek](https://github.com/google/glog), wenn Sie sich für das interessieren, was die Köche des Technologiegiganten verwenden, um ihre Apps zu protokollieren.
- [Die Log4cpp-Bibliothek](http://log4cpp.sourceforge.net/) für einen konfigurierbaren Protokollierungsmechanismus.

Und für ein bisschen Hintergrundlektüre zu den Gründen und Methoden des Protokollierens tauchen Sie ein in:

- Diesen Stack Overflow-Thread über [beste Praktiken beim Protokollieren](https://stackoverflow.com/questions/783956/logging-best-practices) wird Ihnen einen von Experten überprüften intensiven Einblick in das Thema geben.
