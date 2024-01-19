---
title:                "Einen String in Kleinbuchstaben umwandeln"
html_title:           "Elm: Einen String in Kleinbuchstaben umwandeln"
simple_title:         "Einen String in Kleinbuchstaben umwandeln"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Umwandeln eines Strings in Kleinbuchstaben ist eine geläufige Programmieraufgabe, bei der alle Zeichen eines gegebenen Strings in Kleinbuchstaben konvertiert werden. Programmierer tun dies oft, um Textvergleiche zu ermöglichen, die unabhängig von Groß- und Kleinschreibung sind.

## Wie zu:

In PowerShell ist die Konvertierung eines Strings in Kleinbuchstaben ganz einfach. Verwenden Sie die Methode `.ToLower()`. Hier ist ein Beispiel:

```PowerShell
$text = "Hallo Welt"
$lowerCaseText = $text.ToLower()
Write-Host $lowerCaseText
```

Das wird diesen Ausgabe geben:

```PowerShell
hallo welt
```

Die Variable `$lowerCaseText` enthält jetzt den Text 'hallo welt', den Kleinbuchstabenäquivalent des ursprünglichen Texts.

## Vertiefung:

Die Methode `.ToLower()` ist seit der .NET 1.1-Version verfügbar. Sie ist den meisten PowerShell-Programmierern geläufig.

Obwohl `.ToLower()` die häufigste Methode ist, um Strings in Kleinbuchstaben zu konvertieren, es gibt Alternativen. Zum Beispiel könnten Sie die `-replace` Operator mit einem regelmäßigen Ausdruck verwenden, um alle Großbuchstaben durch ihre Kleinbuchstabenequivalente auszutauschen.

Der genaue Algorithmus zur Umwandlung von Groß- in Kleinbuchstaben hängt von der Einstellung Ihrer Sprachkultur ab. Im Deutschen führt `.ToLower()` beispielsweise die spezielle Umwandlung von "ß" durch.

## Mehr Lesen:

 1. Microsoft's Dokumentation zur [ToLower() Methode](https://docs.microsoft.com/de-de/dotnet/api/system.string.tolower?view=net-5.0)
 2. [PowerShell Documentations](https://docs.microsoft.com/de-de/powershell/) für weitere Informationen über PowerShell-Programmierung.