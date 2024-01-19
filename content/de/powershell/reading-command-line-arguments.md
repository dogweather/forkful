---
title:                "Befehlszeilenargumente lesen"
html_title:           "Arduino: Befehlszeilenargumente lesen"
simple_title:         "Befehlszeilenargumente lesen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Mit PowerShell Befehlszeilenargumente lesen

## Was & Warum?

Befehlszeilenargumente sind Eingabeinformationen, die ein Programm beim Start erhält. Programmierer nutzen sie, um die Kontrolle und Flexibilität ihrer Skripte zu erhöhen und um die Benutzerinteraktionen anzupassen.

## Wie geht's:

PowerShell bietet eine einfache Möglichkeit, Befehlszeilenargumente innerhalb eines Skripts zu nutzen. Wir verwenden den speziellen Array-Parameter `$args`.

```PowerShell
# Beispiel Powershell Skript (myscript.ps1)
param(
    [Parameter(Position=0, Mandatory=$true)] [string]$ErstesArgument,
    [Parameter(Position=1, Mandatory=$true)] [string]$ZweitesArgument
)

Write-Output "Erstes Argument ist: $ErstesArgument"
Write-Output "Zweites Argument ist: $ZweitesArgument"
```

Führen Sie das Skript mit Argumenten aus:

```PowerShell
.\myscript.ps1 "Hallo Welt" "Argument Zwei"
```

Ausgabe:

```PowerShell
Erstes Argument ist: Hallo Welt
Zweites Argument ist: Argument Zwei
```

## Tiefere Kenntnisse:

Historisch gesehen gibt es die Verwendung von Befehlszeilenargumenten seit den ersten Tagen der Computer. Sie sind ein universeller Bestandteil vieler Programmiersprachen, inklusive PowerShell.

Alternativen zu `$args` sind `-Args` und `param()`. Der Unterschied besteht darin, dass `-Args` ein Array von Argumenten an eine Funktion oder ein Skript übergibt, während `param()` genutzt wird, um Eingabevariablen in Form von Argumenten für ein Skript zu definieren.

PowerShell behandelt Argumente wie ein Array von Strings, das Sie durchlaufen können, um auf jedes einzelne Argument zuzugreifen. Diese Implementierung ist bahnbrechend, da sie den Zugang zu Argumenten weniger komplex und klarer macht.

## Mehr Infos:

Für tiefere Kenntnisse über Befehlszeilenargumente in PowerShell, checken Sie:

**PowerShell Dokumentation:** 
[Verwenden von Befehlszeilenparametern in Skripten](https://docs.microsoft.com/de-de/powershell/scripting/learn/ps101/09-functions?view=powershell-7.1#using-command-line-parameters-in-your-scripts) 

**StackOverflow Posten:** 
[Wie arbeitet man mit Befehlszeilenargumenten?](https://stackoverflow.com/questions/2157554/powershell-command-line-arguments)

Viel Spaß beim Coden und entdecken der vielseitigen Möglichkeiten von PowerShell!