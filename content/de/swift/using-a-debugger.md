---
title:                "Einsatz eines Debuggers"
date:                  2024-01-26T04:10:28.202713-07:00
model:                 gpt-4-0125-preview
simple_title:         "Einsatz eines Debuggers"

category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/using-a-debugger.md"
---

{{< edit_this_page >}}

## Was & Warum?
Ein Debugger wird eingesetzt, um spezialisierte Werkzeuge zu nutzen, mit denen Ihr Code während der Ausführung getestet und inspiziert werden kann. Das ist eine große Sache, denn es ermöglicht Ihnen, unter die Haube zu schauen, Fehler zu finden und das Verhalten Ihres Codes besser zu verstehen.

## Wie man:
Um den Debugger in Xcode (der IDE für Swift) zu verwenden, können Sie Haltepunkte setzen, Variablen inspizieren und Ausdrücke überwachen. Hier ist ein Beispiel:

```Swift
func findFactorial(of number: Int) -> Int {
    if number == 0 {
        return 1
    }
    return number * findFactorial(of: number - 1)
}

let result = findFactorial(of: 5)
print(result)
```

Setzen Sie einen Haltepunkt, indem Sie links neben einer Zeilennummer in Xcode klicken und das Programm ausführen. Wenn es auf den Haltepunkt trifft, pausiert Xcode die Ausführung. Jetzt können Sie:

1. Variablenwerte überprüfen.
2. Mit den Debugger-Steuerungen übergehen (die nächste Zeile ausführen) oder eintreten (in eine Funktion gehen).
3. Ausdrücke zur 'Überwachungsliste' hinzufügen, um Änderungen an bestimmten Variablen oder Konstanten zu überwachen.

So könnte das, was Sie im Debug-Bereich sehen, aussehen:

```
(lldb) po number
5
(lldb) po result
120
```

## Tiefergehende Betrachtung:
Debugger sind seit den 1940er Jahren Teil der Programmierlandschaft und haben sich von einfachen Haltepunktsystemen zu komplexen, UI-gesteuerten Erlebnissen entwickelt. Neben dem in Xcode integrierten Debugger gibt es auch andere Optionen wie Drittanbieter-Tools wie LLDB (Low Level Debugger), das Xcode unter der Haube verwendet. Einige Leute debuggen sogar mit `print()`-Anweisungen (liebevoll "Höhlenmensch-Debugging" genannt), aber das ist für große Projekte oder komplexe Fehler weniger effizient. Wenn Sie einen Debugger verwenden, jonglieren Sie mit Ausführungskontrolle, Laufzeitintrospektion und Datenmanipulation. Ein tiefes Verständnis dieser Prinzipien trägt weit zu effizientem Debugging bei.

## Siehe auch:
- [Apples Xcode-Debugging-Anleitung](https://developer.apple.com/documentation/xcode/debugging/)
- [LLDB-Schnellstartanleitung](https://lldb.llvm.org/use/tutorial.html)
- [Ray Wenderlichs Swift-Debugging-Tutorial](https://www.raywenderlich.com/966538-arc-and-memory-management-in-swift)
