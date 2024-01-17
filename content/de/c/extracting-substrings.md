---
title:                "Extrahieren von Teilzeichenketten"
html_title:           "C: Extrahieren von Teilzeichenketten"
simple_title:         "Extrahieren von Teilzeichenketten"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## Was & Warum?
Wenn du dich schon einmal mit C-Programmierung beschäftigt hast, hast du vielleicht schon gehört, dass es manchmal nützlich sein kann, Teile eines Strings - also einer Zeichenkette - aus einem bestehenden String herauszunehmen. Diesen Vorgang nennt man auch "Substring-Extraktion". Programmierer nutzen dies, um zum Beispiel bestimmte Informationen aus einem längeren Text herauszufiltern oder um Strings zu manipulieren und zu verarbeiten.

## So Geht's:
Um einen Substring einem String zu extrahieren, kannst du in der Programmiersprache C die Funktion `strncpy()` nutzen. Diese Funktion kopiert eine angegebene Anzahl an Zeichen von einem Source-String in einen Ziel-String. Der folgende Code-Ausschnitt zeigt ein Beispiel dafür, wie man die Funktion benutzt:

```
char *source = "Dies ist ein langer Text.";
char dest[10];
strncpy(dest, source + 5, 10);
printf("%s", dest);
```

Die Ausgabe dieses Codes lautet "ist ein lan".

## Tiefer Tauchen:
Die Möglichkeit, Substrings aus Strings zu extrahieren, ist schon seit den Anfängen der C-Programmierung ein wichtiges Konzept. Früher musste man dafür jedoch kompliziertere Methoden nutzen, wie zum Beispiel das Durchsuchen des Strings nach bestimmten Zeichen oder das manuelle Erstellen eines neuen Strings durch Zusammenfügen einzelner Zeichen. Heutzutage gibt es in C jedoch verschiedene integrierte Funktionen, die diesen Vorgang erleichtern, wie zum Beispiel `strncpy()`, aber auch `strchr()` oder `substr()`. Außerdem gibt es alternative Methoden, wie zum Beispiel Regular Expressions, um Substrings zu extrahieren.

Wenn du dich tiefer in das Thema Substring-Extraktion einarbeiten möchtest, kannst du dir die Dokumentation zu den oben genannten Funktionen anschauen oder auch im Internet nach weiteren Ressourcen suchen.

## Siehe Auch:
- [Dokumentation zu strncpy()](https://www.tutorialspoint.com/c_standard_library/c_function_strncpy.htm)
- [Dokumentation zu strchr()](https://www.tutorialspoint.com/c_standard_library/c_function_strchr.htm)
- [Dokumentation zu substr()](https://www.tutorialspoint.com/functions_in_c/substr.htm)