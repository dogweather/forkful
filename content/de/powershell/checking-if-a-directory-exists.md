---
title:                "Überprüfung, ob ein Verzeichnis existiert"
date:                  2024-01-20T14:58:02.184856-07:00
html_title:           "Fish Shell: Überprüfung, ob ein Verzeichnis existiert"
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Überprüfen, ob ein Verzeichnis existiert, ist eine Methode, um sicherzustellen, dass ein Dateipfad gültig ist, bevor man mit ihm arbeitet. Programmierer machen das, um Fehler zu vermeiden, die auftreten könnten, wenn sie versuchen, auf ein nicht vorhandenes Verzeichnis zuzugreifen.

## So geht's:
Um zu überprüfen, ob ein Verzeichnis in PowerShell existiert, verwenden wir das Cmdlet `Test-Path`. Hier ist ein schnelles Beispiel:

```PowerShell
$verzeichnis = "C:\BeispielOrdner"

if (Test-Path $verzeichnis) {
    "Das Verzeichnis existiert."
} else {
    "Das Verzeichnis existiert nicht."
}
```

Wenn das Verzeichnis existiert, gibt `Test-Path` `$True` zurück, andernfalls `$False`. Hier ist, was Sie erwarten können, wenn Sie das ausführen:

```
Das Verzeichnis existiert.
```

oder

```
Das Verzeichnis existiert nicht.
```

## Deep Dive:
Historisch gesehen verwendet PowerShell das Cmdlet `Test-Path`, das seit den frühen Versionen von PowerShell verfügbar ist. Es ist ein vielseitiges Cmdlet, das nicht nur Verzeichnisse, sondern auch Dateien, Registry-Schlüssel und andere Datenträger prüfen kann.

Alternativ dazu können Sie auch .NET-Möglichkeiten nutzen, wie z.B. `[System.IO.Directory]::Exists($verzeichnis)`, was allerdings weniger PowerShell-idiomatisch ist.

Beim Implementieren ist es wichtig, relative und absolute Pfade korrekt zu behandeln. `Test-Path` kann mit beiden gut umgehen. Allerdings solltest du bei Netzwerkpfaden aufpassen, da es hier zu Zeitverzögerungen oder Zugriffsfehlern kommen kann, die das Ergebnis beeinflussen könnten.

## Siehe auch:
- PowerShell-Dokumentation zu `Test-Path`: https://docs.microsoft.com/powershell/module/microsoft.powershell.management/test-path
- Über das .NET-Objekt `System.IO.Directory`: https://docs.microsoft.com/dotnet/api/system.io.directory.exists
- Mehr über Pfadvalidierung in PowerShell: https://devblogs.microsoft.com/scripting/checking-for-the-existence-of-a-folder-in-powershell/
