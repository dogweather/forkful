---
title:                "Eine Textdatei schreiben"
html_title:           "Arduino: Eine Textdatei schreiben"
simple_title:         "Eine Textdatei schreiben"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Schreiben einer Textdatei bedeutet, Daten in einem lesbaren Format auf das Dateisystem zu speichern. Programmierer tun dies, um Daten zu persistieren, Konfigurationen zu sichern oder Logs zu erstellen.

## How to:

Schreiben einer Textdatei mit "Out-File":
```PowerShell
"Das ist ein Text." | Out-File -FilePath .\Beispiel.txt
```

Anhängen von Text mit "Add-Content":
```PowerShell
Add-Content -Path .\Beispiel.txt -Value "`nEin weiterer Text."
```

Inhalt der Datei "Beispiel.txt" lesen:
```PowerShell
Get-Content .\Beispiel.txt
```

Ausgabe nach dem Ausführen:
```
Das ist ein Text.
Ein weiterer Text.
```

## Deep Dive
Das Speichern von Daten in Textform ist eine der ältesten Arten der Datenpersistenz. Alternativen zum manuellen Schreiben von Dateien beinhalten die Verwendung von höheren Abstraktionen, wie Datenbanken oder Cloud-Speicher. Beim Schreiben in Dateien ist auf Zeichencodierung und Zugriffskonflikte zu achten, insbesondere in Multi-Thread-Umgebungen.

## See Also
- [Microsoft-Dokumentation zu Out-File](https://learn.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/out-file)
- [Microsoft-Dokumentation zu Add-Content](https://learn.microsoft.com/en-us/powershell/module/microsoft.powershell.management/add-content)
