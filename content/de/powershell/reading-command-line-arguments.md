---
title:                "Lesen von Kommandozeilenargumenten"
date:                  2024-01-20T17:56:50.111510-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lesen von Kommandozeilenargumenten"

category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
Kommandozeilenargumente zu lesen heißt, Eingaben von Benutzern zu verarbeiten, die ein Skript ausgeführt haben. Programmierer nutzen das, um Skripts flexibel und interaktiv zu gestalten.

## How to:
Mit PowerShell Argumente zu lesen, ist super einfach. Benutze die automatische Variable `$args`. Hier ein Beispiel:

```PowerShell
# hello.ps1
param(
    [string]$Name = "Welt"
)

Write-Host "Hallo, $Name!"

# Beispielaufruf mit Argument
# PS > .\hello.ps1 -Name "PowerShell Nutzer"

# Beispieloutput:
# Hallo, PowerShell Nutzer!
```

Wenn du `$args` direkt nutzt:

```PowerShell
# direct_args.ps1

# Ergebnis ausgeben
$args.ForEach({
    Write-Host "Argument: $_"
})

# Beispielaufruf ohne parameterübergabe
# PS > .\direct_args.ps1 Das ist cool!

# Beispieloutput:
# Argument: Das
# Argument: ist
# Argument: cool!
```

## Deep Dive
PowerShell Argumente sind nicht neu. Schon in alten Batch-Dateien gabs `%1`, `%2`, usw. PowerShell macht's moderner mit `$args` und `param()`.

Alternativen gibt's natürlich. `$PSBoundParameters` für Named-Parameter und Argument-Transformation durch Funktionen oder Skriptblöcke.

Bei der Implementierung von Kommandozeilenargumenten kommt es darauf an, wie komplex deine Parameterlogik ist. Einfache Skripte kommen oft mit `$args` aus, für mehr Komplexität sind `param()`-Blöcke und explizite Parameterdefinitionen besser.

## See Also
- [Über Parameter und Argumente in der PowerShell](https://ss64.com/ps/syntax-args.html)
- [Microsofts PowerShell Repository auf GitHub](https://github.com/PowerShell/PowerShell)
