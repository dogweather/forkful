---
date: 2024-01-26 01:07:06.225155-07:00
description: "Logging ist im Grunde das Hinterlassen einer Spur innerhalb Ihres Codes\
  \ - es ist die Art und Weise, wie Sie verfolgen, was passiert, wenn Ihr Skript in\u2026"
lastmod: 2024-02-19 22:05:13.041942
model: gpt-4-1106-preview
summary: "Logging ist im Grunde das Hinterlassen einer Spur innerhalb Ihres Codes\
  \ - es ist die Art und Weise, wie Sie verfolgen, was passiert, wenn Ihr Skript in\u2026"
title: Protokollierung
---

{{< edit_this_page >}}

## Was & Warum?
Logging ist im Grunde das Hinterlassen einer Spur innerhalb Ihres Codes - es ist die Art und Weise, wie Sie verfolgen, was passiert, wenn Ihr Skript in der Praxis läuft. Entwickler nutzen das Logging, um Fehler zu finden, das Verhalten von Anwendungen zu verfolgen, die Leistung zu überwachen und um nach Unregelmäßigkeiten Ausschau zu halten.

## Wie man es macht:
Hier sind einige grundlegende Informationen darüber, wie Sie einfaches Logging in Ihre Skripte einbauen:

```PowerShell
# Eine einfache Log-Nachricht erstellen
Write-Host "Info: Der Skriptprozess wird gestartet."

# In eine Datei schreiben
"Info: Das ist eine geloggte Nachricht." | Out-File -Append myLog.log

# Das eingebaute Cmdlet für detaillierteres Logging verwenden
Start-Transcript -Path "./detailedLog.log"
Write-Output "Warnung: Etwas ist nicht ganz richtig."
# ... Ihr Skript macht Sachen
Stop-Transcript

# Ausgabe von detailedLog.log
******************************
Windows PowerShell-Transkript start
Startzeit: 20230324112347
Benutzername  : PShellGuru@example.com
Ausführender Benutzer: PShellGuru@example.com
Konfigurationsname: 
Maschine  : PS-DEVBOX (Microsoft Windows NT 10.0.17763.0)
Host-Anwendung: C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe
Prozess-ID: 2024
PS-Version: 7.1.2
```

Jetzt gibt es in Ihren Logs eine Spiel-für-Spiel-Beschreibung dessen, was Ihr Code gemacht hat.

## Tiefgreifende Einblicke:
Historisch gesehen ist das Logging fast so alt wie die Programmierung selbst. Es ist wie das Logbuch eines Kapitäns, nur für Software. Früher hätte es Druckausgaben oder Fernschreibermaschinen sein können; heute geht es um Dateien und ausgefeilte Log-Management-Systeme.

Wenn Sie tief in den PowerShell-Gräben stecken, ist `Write-Host` schnell und schmutzig, es spuckt jedoch nur Text auf die Konsole aus und ist nicht großartig für Aufzeichnungen. Mit `Out-File` haben Sie eine einfache Möglichkeit, Text in eine Datei zu schreiben, aber für den richtigen Saft werden Sie `Start-Transcript` und `Stop-Transcript` verwenden wollen, die alles loggen – Eingaben, Ausgaben, das ganze Programm.

Alternativen? Sicher, wenn Sie im Unternehmensbereich arbeiten, könnten Sie sich das Windows-Ereignisprotokoll ansehen oder Software wie Logstash verwenden, aber für Ihr tägliches Skript bleiben Sie bei den Tools von PowerShell. Was die Umsetzung betrifft, erinnern Sie sich daran, schlau zu loggen – zu wenig und es ist nutzlos, zu viel und es ist nur weißes Rauschen.

## Siehe auch:
Schauen Sie sich diese an, um alles über Logging in PowerShell in den Griff zu bekommen:
