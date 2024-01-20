---
title:                "Einen String großschreiben"
html_title:           "PowerShell: Einen String großschreiben"
simple_title:         "Einen String großschreiben"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# PowerShell: String-Transformation - Wie man Strings in Großbuchstaben umwandelt

## Was & Warum?

Das Umwandeln eines Strings in Großbuchstaben, auch als Capitalizing bekannt, ist eine gängige Operation in der Textbearbeitung. Programmierer nutzen dies, um die Lesbarkeit zu verbessern oder um Textvergleiche unempfindlich gegenüber Groß- und Kleinschreibung zu gestalten.

## Anleitung:

In PowerShell geschieht dies mittels der `.ToUpper()` Funktion. Hier ist ein einfaches Beispiel:

```PowerShell
$var = "hallo welt"
$upperCaseVar = $var.ToUpper()
Write-Host $upperCaseVar
```

Die Ausgabe davon wäre:

```PowerShell
HALLO WELT
```

## Deep Dive:

Das Capitalizing von Strings hat lange Tradition und kann bis zu den Anfängen der Textverarbeitungsystems zurückverfolgt werden. Es gibt Alternativen wie zum Beispiel die Funktion `.ToLower()`, die alle Zeichen eines Strings in Kleinbuchstaben umwandelt. Diese hilft ebenfalls dabei, Textvergleiche durchzuführen, die unempfindlich gegenüber Groß- und Kleinschreibung sind. Bei der Implementierung von `.ToUpper()` in PowerShell handelt es sich eigentlich um eine Methode des String-Typs im .NET Framework, das die Grundlage für PowerShell bildet.

## Siehe auch:

- [PowerShell-String-Manipulationsmethoden](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_string?view=powershell-7.1)
- [.NET Framework String.ToUpper Methode](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper)
- [PowerShell Grundlagen für die Textverarbeitung](https://4sysops.com/archives/powershell-string-manipulation-guide/)