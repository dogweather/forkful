---
title:                "C: Schreiben in die Standardfehlerausgabe."
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Warum

Das Schreiben in den Standardfehler ist ein wichtiger Aspekt der C-Programmierung, der oft übersehen wird. Es ist jedoch eine wichtige Methode, um Fehler und Debugging-Probleme zu identifizieren und zu beheben. Durch das Schreiben in den Standardfehler können Sie wichtige Informationen über den Status und die Ausführung Ihres Programms erhalten, die nicht durch das normale Drucken von Ausgaben angezeigt werden. In diesem Blogbeitrag werden wir uns mit der grundlegenden Verwendung von Standardfehler in C befassen und warum es wichtig ist, dies zu tun.

# Wie man es macht

Um in den Standardfehler zu schreiben, müssen Sie die Funktion "fprintf" verwenden, die speziell zum Schreiben von Text in den angegebenen Stream entwickelt wurde. In diesem Fall wird der Standardfehler-Stream durch die Verwendung von "stderr" als Parameter angegeben. Hier ist ein Beispielcode:

```
#include <stdio.h>

int main()
{
    fprintf(stderr, "Dies ist ein Fehler!");
    return 0;
}
```

Das obige Programm wird einfach den Text "Dies ist ein Fehler!" in den Standardfehler schreiben, wenn es ausgeführt wird. Beachten Sie, dass es wichtig ist, die übergeordnete Funktion "fprintf" zu verwenden, da dies dafür sorgt, dass der Text korrekt im Standardfehlerformat angezeigt wird.

Der Standardfehler ist besonders nützlich, wenn Sie versuchen, Fehler in Ihrem Programm zu identifizieren, da es Ihnen ermöglicht, spezifische Informationen über die Fehlerursachen zu erhalten.

# Tiefer tauchen

Die Verwendung von Standardfehler ist nicht auf einfache Fehlermeldungen beschränkt. Sie können auch Formatierungsoptionen wie "printf" verwenden, um die Ausgabe zu verbessern. Darüber hinaus können Sie auch spezielle Funktionen wie "errno" und "strerror" verwenden, um detaillierte Informationen über die Fehler zu erhalten.

Da Fehlermeldungen oft auf unvorhergesehene Probleme hinweisen, können Sie mit der Verwendung von Standardfehler Ihre Codebasis verbessern, indem Sie die Fehlerbehandlungspfade aktualisieren oder erweitern. Dies kann dazu beitragen, zukünftige Fehler zu vermeiden und die Stabilität Ihres Programms zu verbessern.

# Siehe auch

* [Offizielle Dokumentation zu Standardfehlern in C](https://www.gnu.org/software/libc/manual/html_node/Standard-Error-Streams.html)
* [Fehlerbehandlung im C-Programmierung-Wiki](https://en.wikipedia.org/wiki/Error_handling)
* [Verwenden von Standardfehlern in Python](https://docs.python.org/3.7/library/sys.html#sys.stderr)