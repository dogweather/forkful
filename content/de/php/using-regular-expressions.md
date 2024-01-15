---
title:                "Verwendung von regulären Ausdrücken"
html_title:           "PHP: Verwendung von regulären Ausdrücken"
simple_title:         "Verwendung von regulären Ausdrücken"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum

Reguläre Ausdrücke sind ein leistungsstarkes Werkzeug zur Mustererkennung in Texten und ermöglichen es Entwicklern, komplexe Such- und Ersetzungsvorgänge schnell und effizient durchzuführen. Sie sind in der PHP-Programmierung unverzichtbar und können in verschiedenen Anwendungsfällen eingesetzt werden, einschließlich Validierung von Benutzereingaben, Parsing von Daten oder Änderung von Textformaten.

## Wie geht's

Um reguläre Ausdrücke in PHP zu verwenden, müssen Sie die integrierten Funktionen und Operators verstehen, die für die Arbeit mit regulären Ausdrücken zur Verfügung stehen. Zum Beispiel können Sie mit der Funktion ```preg_match()``` überprüfen, ob ein bestimmtes Muster in einem String gefunden wird. Hier ist ein einfaches Beispiel:

```PHP
$string = "Hallo, mein Name ist Maria und ich bin 27 Jahre alt.";
if(preg_match("/Maria/", $string)){
  echo "Das Muster wurde gefunden!";
}
else{
  echo "Das Muster wurde nicht gefunden";
}
```

Dieses Codebeispiel sucht nach dem Wort "Maria" in dem String und gibt "Das Muster wurde gefunden!" aus. Wenn das Wort nicht gefunden werden würde, würde der zweite Teil der Bedingung ausgeführt werden.

Sie können auch reguläre Ausdrücke verwenden, um Teile eines Strings zu extrahieren oder zu ersetzen. Mit der Funktion ```preg_replace()``` können Sie beispielsweise alle Vorkommen eines bestimmten Musters in einem String ersetzen. Hier ist ein Beispiel:

```PHP
$string = "Hello, World!";
$newString = preg_replace("/Hello/", "Hi", $string);
echo $newString; // "Hi, World!"
```

In diesem Beispiel wird das Wort "Hello" im String durch "Hi" ersetzt, was zur Ausgabe von "Hi, World!" führt.

## Deep Dive

Reguläre Ausdrücke können komplexe Muster enthalten und einige Sonderzeichen, die für spezielle Funktionen stehen. Hier sind einige der häufig verwendeten Sonderzeichen:

- ```^```: Steht für den Anfang eines Strings
- ```$```: Steht für das Ende eines Strings
- ```*```: Steht für null oder mehr Vorkommen des vorherigen Zeichens
- ```+```: Steht für ein oder mehr Vorkommen des vorherigen Zeichens
- ```?```: Steht für null oder ein Vorkommen des vorherigen Zeichens
- ```.```: Steht für ein beliebiges Zeichen außer einer neuen Zeile
- ```\s```: Steht für ein Leerzeichen, Tabulatorzeichen oder eine neue Zeile
- ```\d```: Steht für eine Ziffer
- ```\w```: Steht für ein alphanumerisches Zeichen oder Unterstrich

Es gibt auch weitere Funktionen und Optionen, die mit regulären Ausdrücken in PHP verwendet werden können. Es ist wichtig, sich ausführlicher mit der Syntax und den Möglichkeiten von regulären Ausdrücken zu befassen, um sie effektiv einzusetzen.

## Siehe auch

- [PHP reguläre Ausdrücke Referenz](https://www.php.net/manual/de/reference.pcre.pattern.syntax.php)
- [Regex Tutorial auf PHP.net](https://www.php.net/manual/de/regexp.reference.php)
- [RegExr - Online Regular Expression Tester](https://regexr.com/)