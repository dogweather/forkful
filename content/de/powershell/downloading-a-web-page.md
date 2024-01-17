---
title:                "Herunterladen einer Webseite"
html_title:           "PowerShell: Herunterladen einer Webseite"
simple_title:         "Herunterladen einer Webseite"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Herunterladen einer Website mit PowerShell

## Was & Warum?

Das Herunterladen einer Webseite bedeutet, die Website und ihre Inhalte auf einen lokalen Computer zu kopieren, um sie offline nutzen zu können oder die Daten zu analysieren. Programmierer nutzen diese Methode häufig, um Daten aus einer Website zu extrahieren und weiterzuverarbeiten.

## Wie geht's?

Herunterladen einer Webseite mit PowerShell ist einfach und unkompliziert. Dazu musst du lediglich folgende Befehle in deiner PowerShell-Konsole ausführen:

`$client = New-Object System.Net.WebClient`
`$url = "https://www.example.com"`
`$client.DownloadFile($url, "C:\Users\Username\Downloads\example.html")`

Dieser Code erstellt einen Webclient, der das Herunterladen von Dateien aus dem Internet ermöglicht. Anschließend wird die URL der Webseite angegeben und der Befehl `$client.DownloadFile` verwendet, um die Datei auf deinen Computer herunterzuladen.

Die heruntergeladene Datei wird dann im angegebenen Speicherort gespeichert, in diesem Fall im Ordner "Downloads" unter deinem Benutzernamen.

## Tiefere Einblicke

Historisch gesehen wurde das Herunterladen von Webseiten hauptsächlich für die Offline-Nutzung verwendet, beispielsweise beim Lesen von E-Books oder beim Erstellen von CD-ROM-Sammlungen von Websites. Heutzutage wird es jedoch auch häufig für das Daten-Scraping verwendet, um Informationen von Websites zu extrahieren und in anderen Anwendungen zu verwenden.

Es gibt auch alternative Möglichkeiten, um eine Webseite mit PowerShell herunterzuladen, wie zum Beispiel den Befehl `Invoke-WebRequest`. Dieser ist jedoch etwas komplexer als der oben genannte Ansatz.

## Siehe auch

- [PowerShell-Referenz für WebClient](https://docs.microsoft.com/en-us/dotnet/api/system.net.webclient?view=netcore-3.1)
- [Offizielle PowerShell-Dokumentation](https://docs.microsoft.com/en-us/powershell/)
- [Tutorial zum Herunterladen von Dateien mit PowerShell](https://www.youtube.com/watch?v=qnZi2jE_n5Q)