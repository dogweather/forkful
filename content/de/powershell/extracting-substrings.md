---
title:                "Unterstrings extrahieren"
html_title:           "PowerShell: Unterstrings extrahieren"
simple_title:         "Unterstrings extrahieren"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Was & Warum?
Extrahieren von Teilzeichenfolgen ist die Praxis des Extrahierens von bestimmten Zeichen aus einer längeren Zeichenfolge basierend auf bestimmten Regeln oder Kriterien. Programmierer nutzen diese Technik häufig, um Daten zu filtern oder zu bearbeiten, um sie für bestimmte Zwecke zu formatieren oder zu analysieren.

## Wie geht's?
Das Extrahieren von Teilzeichenfolgen ist in PowerShell einfach mit dem Befehl ```$string.SubString(startIndex, length)``` durchzuführen. Der Befehl gibt die Teilzeichenfolge zurück, die an der angegebenen Startposition beginnt und die angegebene Länge hat.

Beispiel:
```PowerShell
$string = "PowerShell ist der beste! 😎"
$newString = $string.SubString(0, 9)
$newString # Ausgabe: PowerShell

$newString = $string.SubString(18, 3)
$newString # Ausgabe: 😎
```

## Tiefer Einblick
Das Extrahieren von Teilzeichenfolgen ist eine gängige Methode in vielen programmierbaren Sprachen, einschließlich PowerShell. Es ist besonders nützlich bei der Manipulation von Zeichenfolgen und wird oft in Kombination mit anderen Befehlen verwendet, um komplexere Aufgaben zu erledigen.

Alternativen zum Extrahieren von Teilzeichenfolgen können Regex-Ausdrücke oder das Splitten von Zeichenfolgen basierend auf bestimmten Trennzeichen sein. In PowerShell gibt es auch spezielle Befehle wie ```Select-String```, die das Extrahieren von Teilzeichenfolgen in einer Zeichenfolge erleichtern können.

Bei der Implementierung des Extrahierens von Teilzeichenfolgen in PowerShell gibt es einige Dinge zu beachten, wie z.B. die Verwendung von Index 0 als Startposition, wenn die Nummerierung der Zeichenfolgen in PowerShell bei 0 beginnt.

## Sieh dir auch an
Für weitere Informationen und Beispiele zum Extrahieren von Teilzeichenfolgen in PowerShell, sieh dir die folgenden Quellen an:

- [Microsoft Dokumentation: String.SubString Methode](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring?view=netcore-3.1)
- [SS64: Substring](https://ss64.com/ps/substring.html)
- [TechNet: Powershell String Manipulation with Substring](https://social.technet.microsoft.com/wiki/contents/articles/26418.powershell-string-manipulation-with-substring.aspx)