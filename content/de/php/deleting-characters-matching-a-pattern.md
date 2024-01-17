---
title:                "Löschen von Zeichen mit Übereinstimmungsmuster"
html_title:           "PHP: Löschen von Zeichen mit Übereinstimmungsmuster"
simple_title:         "Löschen von Zeichen mit Übereinstimmungsmuster"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Entfernen von Zeichen, die einem bestimmten Muster entsprechen, ist eine gängige Aufgabe in der Programmierung. Es ermöglicht Programmierern, unerwünschte Zeichen in einer Zeichenkette zu entfernen und diese zu bereinigen.

Diese Aufgabe ist besonders nützlich, um Eingaben von Benutzern zu validieren oder um Daten zu filtern, bevor sie in eine Datenbank gespeichert werden. Programmierer nutzen das Löschen von Zeichen, die einem Muster entsprechen, um sicherzustellen, dass die Daten in einem bestimmten Format vorliegen und ungewollte Zeichen entfernt wurden.

## Wie man es macht:

```PHP
// Beispiel einer Zeichenkette mit ungewollten Zeichen
$string = "08-01-2021!!";

// Löscht alle nicht-numerischen Zeichen aus der Zeichenkette
echo preg_replace("/[^0-9]/", "", $string);

// Ausgabe: 08012021
```

In diesem Beispiel wird die Funktion `preg_replace()` verwendet, um alle Zeichen, die nicht dem numerischen Muster entsprechen, aus der Zeichenkette zu entfernen. Dies ermöglicht es uns, eine formatierte Datumseingabe zu erhalten, die sich leicht in einer Datenbank speichern lässt.

Es ist auch möglich, ein bestimmtes Muster anzugeben, dem die Zeichen entsprechen müssen, um entfernt zu werden. Die Verwendung von regulären Ausdrücken gibt Programmierern die Flexibilität, das Löschen von Zeichen an ihre Bedürfnisse anzupassen.

## Tiefergehende Informationen:

### Historischer Kontext:

Das Entfernen von Zeichen, die einem Muster entsprechen, ist eine Technik, die in der Programmierung schon seit langem verwendet wird. Sie ist eng mit der Verwendung von regulären Ausdrücken verbunden, die bereits in den 1950er Jahren entwickelt wurden. Diese Werkzeuge haben es Programmierern ermöglicht, komplexere Such- und Ersetzungsaufgaben durchzuführen, einschließlich des Löschens von Zeichen.

### Alternativen:

Obwohl die Verwendung von regulären Ausdrücken die gängigste Methode zum Löschen von Zeichen ist, gibt es auch andere Möglichkeiten, diese Aufgabe zu erledigen. Zum Beispiel können Programmierer spezifische Funktionen in ihrer Programmiersprache nutzen, um Zeichen zu entfernen, anstatt reguläre Ausdrücke zu verwenden. Jede Methode hat ihre Vor- und Nachteile und es liegt an dem Programmierer, die beste Lösung für seine spezifische Situation zu wählen.

### Implementierungsdetails:

Die Implementierung des Löschen von Zeichen, die einem Muster entsprechen, kann je nach Programmiersprache und verwendeter Funktion variieren. Es ist wichtig, genau zu verstehen, welche Zeichen entfernt werden und wie das Muster interpretiert wird, um unerwünschte Ergebnisse zu vermeiden. Eine sorgfältige Testung der Funktion vor dem Einsatz in einer Produktionsumgebung ist daher ratsam.

## Siehe auch:

- [PHP Dokumentation zur Funktion preg_replace()](https://www.php.net/manual/en/function.preg-replace.php)
- [Einführung in reguläre Ausdrücke in der Programmierung](https://www.regular-expressions.info/tutorial.html)