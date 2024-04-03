---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:42.579423-07:00
description: "Wie: PowerShell bietet eine einfache M\xF6glichkeit, die Existenz eines\
  \ Verzeichnisses mit dem Cmdlet `Test-Path` zu \xFCberpr\xFCfen. Dieses Cmdlet gibt\
  \ einen\u2026"
lastmod: '2024-03-13T22:44:54.118036-06:00'
model: gpt-4-0125-preview
summary: "PowerShell bietet eine einfache M\xF6glichkeit, die Existenz eines Verzeichnisses\
  \ mit dem Cmdlet `Test-Path` zu \xFCberpr\xFCfen."
title: "\xDCberpr\xFCfung, ob ein Verzeichnis existiert"
weight: 20
---

## Wie:
PowerShell bietet eine einfache Möglichkeit, die Existenz eines Verzeichnisses mit dem Cmdlet `Test-Path` zu überprüfen. Dieses Cmdlet gibt einen booleschen Wert zurück, der angibt, ob der angegebene Pfad existiert. So können Sie es verwenden:

```powershell
# Überprüfen, ob ein Verzeichnis existiert
$directoryPath = "C:\ExamplePath"
$directoryExists = Test-Path -Path $directoryPath
Write-Output "Existiert das Verzeichnis? $directoryExists"
```

Beispielausgabe für ein existierendes Verzeichnis:

```
Existiert das Verzeichnis? True
```

Und für ein nicht existierendes Verzeichnis:

```
Existiert das Verzeichnis? False
```

Für komplexere Skripte, insbesondere solche, die mit Netzwerkfreigaben oder Cloud-Speicher interagieren, könnten zusätzliche Überprüfungen oder Funktionalitäten benötigt werden, die nicht direkt durch `Test-Path` verfügbar sind. In solchen Fällen könnte die Nutzung von Drittanbieter-PowerShell-Modulen oder -Bibliotheken von Vorteil sein, obwohl die meisten routinemäßigen Aufgaben mit den integrierten Cmdlets von PowerShell erledigt werden können. Bis zu meinem letzten Wissensstand gab es keine weit verbreitete Drittanbieter-Bibliothek speziell für die Überprüfung der Existenz von Verzeichnissen über das hinaus, was `Test-Path` bietet, hauptsächlich weil `Test-Path` selbst sowohl robust als auch effizient für diesen Zweck ist.
