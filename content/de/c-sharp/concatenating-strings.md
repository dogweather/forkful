---
title:                "Verbinden von Zeichenfolgen"
html_title:           "C#: Verbinden von Zeichenfolgen"
simple_title:         "Verbinden von Zeichenfolgen"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Was & Warum?
Bei der String-Verkettung werden mehrere Zeichenfolgen zu einer einzigen kombiniert. Programmierer tun dies, um effizienter und schneller auf Daten zuzugreifen, da der zusammengefügte String nur einmal im Speicher gespeichert werden muss und somit weniger Speicherplatz belegt. 

## Wie geht's?
```C#
string string1 = "Hallo ";
string string2 = "Welt!";
string result = string1 + string2;
Console.WriteLine(result);
```
Ausgabe: 
```
Hallo Welt!
```

## Tiefere Einblicke
Die String-Verkettung ist eine grundlegende Funktion in den meisten Programmiersprachen, die schon seit den Anfängen der Computerprogrammierung existiert. Alternativ kann auch der `String.Format()`-Befehl verwendet werden, um Strings zu kombinieren. Die String-Verkettung ist effizienter als die Verwendung von `+` oder `+=` für mehrere Verkettungen, da es weniger temporäre Variablen erzeugt und somit den Speicherplatz und die Verarbeitungszeit verringert.

## Siehe auch
Weitere Informationen zur String-Verkettung in C# finden Sie in der offiziellen Microsoft-Dokumentation und in verschiedenen Online-Tutorials.