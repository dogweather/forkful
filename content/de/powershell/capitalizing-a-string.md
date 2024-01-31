---
title:                "String in Großbuchstaben umwandeln"
date:                  2024-01-19
simple_title:         "String in Großbuchstaben umwandeln"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Großschreiben eines Strings ist die Umwandlung aller Buchstaben in Großbuchstaben. Programmierer verwenden dies, um Konsistenz in Benutzereingaben zu gewährleisten oder um Schriftzüge hervorzuheben.

## Vorgehensweise:
Um in PowerShell einen String zu großzuschreiben, nutzt du die `.ToUpper()` Methode:

```PowerShell
$text = "hallo welt"
$capitalizedText = $text.ToUpper()
Write-Host $capitalizedText
```

Ergebnis:

```
HALLO WELT
```

Ein weiteres Beispiel, wenn man nur den ersten Buchstaben großschreiben möchte:

```PowerShell
$text = "hallo welt"
$capitalizedText = $text.Substring(0,1).ToUpper()+$text.Substring(1)
Write-Host $capitalizedText
```

Ergebnis:

```
Hallo welt
```

## Vertiefung:
Das Großschreiben von Strings war schon immer ein Teil von Programmiersprachen, um Text formatieren zu können. In PowerShell verwendet man `.ToUpper()` zum Großschreiben kompletter Strings oder `.Substring()` in Kombination mit `.ToUpper()`, um nur den ersten Buchstaben zu großzuschreiben.

Es gibt Alternativen, wie etwa kulturell spezifische Methoden, die beim Großschreiben Buchstaben entsprechend der Sprachregeln verändern, zum Beispiel in der türkischen Sprache.

In älteren Programmiersprachen oder bei sehr spezifischen Anforderungen könnte man jeden Buchstaben einzeln durchlaufen und in einen Großbuchstaben umwandeln. Dies ist aber in PowerShell selten erforderlich.

## Siehe Auch:
- [PowerShell Dokumentation zur String-Klasse](https://docs.microsoft.com/de-de/dotnet/api/system.string?view=netcore-3.1)
- [StackOverflow: Großschreibung in PowerShell](https://stackoverflow.com/questions/tagged/powershell+uppercase)
- [Microsoft Dokumentation: Kulturinformationen in .NET](https://docs.microsoft.com/de-de/dotnet/api/system.globalization.cultureinfo?view=netcore-3.1)
