---
title:                "String in Großbuchstaben umwandeln"
html_title:           "PHP: String in Großbuchstaben umwandeln"
simple_title:         "String in Großbuchstaben umwandeln"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Du hast sicher schon mal davon gehört, eine Zeichenkette (oder String) zu "capitalizen". Aber warum sollte man das überhaupt tun? Nun, es gibt verschiedene Gründe, warum du möglicherweise in deiner PHP-Programmierung eine Zeichenkette großschreiben möchtest. Einer der häufigsten Gründe ist die Formatierung von Benutzereingaben, um sicherzustellen, dass jeder Eingabe sowie Ausgabe in Großbuchstaben beginnt. Dies kann besonders hilfreich sein, wenn du mit Datenbanken arbeitest und sicherstellen möchtest, dass alle Einträge in einheitlichem Format gespeichert werden.

## Wie

Um eine Zeichenkette in Großbuchstaben zu schreiben, gibt es in PHP eine praktische Funktion namens "strtoupper()". Diese Funktion wandelt alle Kleinbuchstaben in der Zeichenkette in Großbuchstaben um. Schauen wir uns dazu ein Beispiel an:

```PHP
$string = "hallo welt";
echo strtoupper($string);
```
Ergebnis: HALLO WELT

Wie du siehst, wird der gesamte Text automatisch in Großbuchstaben ausgegeben. Aber was ist, wenn du nur den ersten Buchstaben einer Zeichenkette großschreiben möchtest? Dafür gibt es die Funktion "ucfirst()", die den ersten Buchstaben eines Strings in Großbuchstaben umwandelt. Hier ist ein Beispiel:

```PHP
$string = "guten morgen";
echo ucfirst($string);
```
Ergebnis: Guten morgen

Wie du sehen kannst, bleibt der Rest der Zeichenkette in Kleinbuchstaben, während der erste Buchstabe großgeschrieben wird.

## Deep Dive

Jetzt, da du gesehen hast, wie einfach es ist, eine Zeichenkette in PHP zu capitalizen, werfen wir einen Blick auf die technischen Details dahinter. In PHP wird der Inhalt einer Variablen als Datentyp "String" gespeichert. Jeder Buchstabe in einer Zeichenkette hat einen bestimmten Code, der ihm zugeordnet ist. Diese Codes werden in einer sogenannten "Character Encoding Table" aufgeführt, die festlegt, welcher Code welchem Buchstaben entspricht. Wenn du mit der Funktion "strtoupper()" oder "ucfirst()" auf eine Zeichenkette zugreifst, wird dieser Code verwendet, um den Buchstaben in Großbuchstaben umzuwandeln.

Dabei ist es wichtig zu beachten, dass dies von der aktuellen Systemeinstellung abhängt. Je nach verwendetem Betriebssystem oder Spracheinstellung kann die "Character Encoding Table" variieren. Deshalb ist es ratsam, immer zu überprüfen, ob die Ausgabe deiner capitalizierten Zeichenkette korrekt ist.

## Siehe auch

- PHP-Manual: String-Funktionen (https://www.php.net/manual/de/ref.strings.php)
- YouTube-Video: String Manipulation in PHP (https://www.youtube.com/watch?v=wfk337sE_VQ)
- Artikel: Character Encoding in PHP (https://www.phptherightway.com/#character_encoding)