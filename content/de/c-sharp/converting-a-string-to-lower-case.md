---
title:                "Einen String in Kleinbuchstaben umwandeln"
html_title:           "Elm: Einen String in Kleinbuchstaben umwandeln"
simple_title:         "Einen String in Kleinbuchstaben umwandeln"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Konvertierung von Zeichenketten (Strings) in Kleinbuchstaben ist eine gängige Operation in der Programmierung. Sie ermöglicht es, irrelevante Unterschiede im Schriftbild zu eliminieren und vereinfacht z. B. den Textvergleich.

## Wie es geht:
```C#
string originalText = "Hallo Welt!";
string lowercaseText = originalText.ToLower();

Console.WriteLine(lowercaseText);
```
Bei Ausführung gibt dieses Programm "hallo welt!" aus.

## Tiefgang:
Die Funktion ToLower() hat eine lange Geschichte, die bis in die Anfänge der C#-Sprache zurückreicht. Es gibt Alternativen wie `ToLowerInvariant()`, die jedoch subtile Unterschiede in der Handhabung von Sprachspezifikationen aufweisen. Für die meisten Anwendungsfälle ist `ToLower()` jedoch die richtige Wahl. 

Bei der Umsetzung speichert die Methode `ToLower()` zuerst die ursprüngliche Zeichenkette und wandelt dann jeden Buchstaben einzeln in einen Kleinbuchstaben um. Dies macht sie zu einer sicheren, wenn auch nicht sehr effizienten, Methode für diese Umwandlung.

## Siehe auch:
1. [Microsoft Dokumentation zur ToLower()-Methode](https://docs.microsoft.com/de-de/dotnet/api/system.string.tolower?view=net-5.0)
2. [Diskussion über ToLower() vs ToLowerInvariant()](https://stackoverflow.com/questions/2801508/string-tolowerstring-tolowerinvariant)
3. [Detaillierte Analyse der ToLower()-Implementierung](https://referencesource.microsoft.com/#mscorlib/system/string.cs,1975)