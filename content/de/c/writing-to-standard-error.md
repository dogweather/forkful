---
title:                "Schreiben auf den Standardfehler"
html_title:           "C: Schreiben auf den Standardfehler"
simple_title:         "Schreiben auf den Standardfehler"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Was ist das Schreiben auf den Standardsfehler und warum machen es Programmierer?

Das Schreiben auf den Standardsfehler (englisch: Standard Error), auch bekannt als stderr, ist ein Weg für Programmierer, Fehler- und Diagnoseinformationen während der Programm-Ausführung auszugeben. Durch das Schreiben auf stderr können Programmierer Fehler in ihrem Code identifizieren und beheben, was letztendlich zu einer besseren und stabileren Software führt.

# Wie geht das?

Das Schreiben auf den Standardsfehler ist in C sehr einfach. Mit der Funktion `fprintf` kann man Nachrichten und Variablenwerte direkt auf stderr ausgeben. Hier ist ein Beispiel, welches einen Fehler meldet, wenn eine Datei nicht geöffnet werden kann:

```C
int main(){
    FILE* file;
    file = fopen("meine_datei.txt", "r");
    if (file == NULL) {
        fprintf(stderr, "Fehler beim Öffnen der Datei!");
        return 1;
    }
    // weiterer Code
}
```

Wenn die Datei `meine_datei.txt` nicht geöffnet werden kann, wird die Fehlermeldung "Fehler beim Öffnen der Datei!" auf stderr ausgegeben. Diese Nachricht kann dann von einem Entwickler gelesen werden, um den Fehler zu finden und zu beheben. 

# Tiefer tauchen

Während das Schreiben auf stderr ein sehr einfaches und nützliches Werkzeug für Programmierer ist, gibt es auch alternative Methoden, um Fehlermeldungen auszugeben. Eine bekannte Alternative ist das Schreiben auf den Standardsausgang (englisch: Standard Output), oder stdout. Hier werden Nachrichten als Teil der normalen Programmausgabe ausgegeben. Dies kann nützlich sein, um Benutzer über den Fortschritt des Programms zu informieren. 

Es ist auch wichtig zu beachten, dass stderr und stdout in der Regel nicht direkt vom Programmierer gelesen werden können. Stattdessen werden sie in einem Terminal oder einer virtuellen Konsole ausgegeben, wo der Benutzer sie lesen kann. Eine weitere wichtige Eigenschaft von stderr ist, dass es ungepuffert bleibt, was bedeutet, dass Nachrichten sofort ausgegeben werden, im Gegensatz zu stdout, wo sie in einem Buffer gespeichert und später ausgegeben werden. 

Zusätzlich zur Verwendung von `fprintf` gibt es auch die Möglichkeit, den Präprozessorbefehl `#error` zu verwenden, um Fehlermeldungen während der Übersetzung des Programms zu erzeugen. Dies kann nützlich sein, um beispielsweise auf bestimmte Plattformen oder Compiler-Versionen hinzuweisen, die nicht unterstützt werden.

# Weiterlesen

- [Offizielle C-Dokumentation zu `fprintf`](https://www.cplusplus.com/reference/cstdio/fprintf/)
- [Eine Einführung in die Standard Streams in C](https://www.codingame.com/playgrounds/14213/understanding-c-inputs-and-outputs)
- [Detaillierter Artikel über stderr und stdout in C](https://www.geeksforgeeks.org/understanding-stderr-stdout-linux/)