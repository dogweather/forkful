---
title:                "Ausgabe von Fehlermeldungen drucken"
html_title:           "Lua: Ausgabe von Fehlermeldungen drucken"
simple_title:         "Ausgabe von Fehlermeldungen drucken"
programming_language: "Lua"
category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/printing-debug-output.md"
---

{{< edit_this_page >}}

## Was & Warum?
Debug-Ausgaben zu drucken ist ein gebräuchlicher Prozess unter Programmierern. Es hilft bei der Fehlersuche und bietet Einblicke in den Programmablauf, insbesondere während der Entwicklung.

## Wie geht's?
Debug-Ausgaben können in Lua ganz einfach durch die Verwendung von ```print```-Funktionen gemacht werden. Zum Beispiel:
```Lua
myNumber = 5
print("Die Variable myNumber hat den Wert:", myNumber)
```
Das obige Code-Beispiel würde die folgende Ausgabe erzeugen:
```
Die Variable myNumber hat den Wert: 5
```
Es ist auch möglich, Debug-Ausgaben in Kombination mit Variablen während des Programmablaufs zu verwenden, um den Wert oder Status von Variablen zu überwachen. Zum Beispiel:
```Lua
myCount = 0 
myCount = myCount + 1 
print("myCount hat den Wert:", myCount) 
```
Dies würde bei jeder Ausführung die Anzahl der Male, die die Variable `myCount` erhöht wurde, ausgeben.

## Tief eintauchen
Debug-Ausgaben haben in der Programmierung eine lange Geschichte und wurden verwendet, lange bevor Debugger oder andere komplexe Debugging-Tools existierten. Während es andere Möglichkeiten gibt, Debugging-Aufgaben durchzuführen, ist das Drucken von Debug-Ausgaben immer noch eine einfache und effektive Methode, um während des Programmierprozesses Einblicke zu erhalten.

## Siehe auch
- [Lua 5.4 Dokumentation](https://www.lua.org/manual/5.4/) für weitere Informationen über `print`-Funktionen und Debugging in Lua.
- [Lua Debug Library](https://www.lua.org/manual/5.4/manual.html#6.10) für fortgeschrittenere Debugging-Techniken in Lua.