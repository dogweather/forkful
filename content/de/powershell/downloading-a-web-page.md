---
title:                "Eine Webseite herunterladen"
html_title:           "Arduino: Eine Webseite herunterladen"
simple_title:         "Eine Webseite herunterladen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Was & Warum?

Webseiten herunterzuladen bedeutet, die HTML-Daten einer Webseite auf Ihren lokalen Rechner zu kopieren. Programmierer tun dies, um Daten zu sammeln, die Analyse zu automatisieren, Tests durchzuführen oder Offline-Kopien zu erstellen.

## So geht's:

Mit PowerShell können wir die `Invoke-WebRequest` Funktion verwenden, um eine Webseite herunterzuladen. Hier ist ein einfaches Beispiel:

```PowerShell
$url = "http://beispiel.de"
$response = Invoke-WebRequest -Uri $url
$response.Content > "beispiel.html"
```

Wenn Sie dies ausführen, wird die HTML-Inhalte der Webseite "beispiel.de" in die Datei "beispiel.html" gespeichert.

## Deep Dive

PowerShell ist die Nachfolgeversion von Microsofts Command Prompt und wurde erstmals 2006 eingeführt. Im Gegensatz zu seinem Vorgänger unterstützt PowerShell .Net und hat eine viel größere Palette an Funktionen, einschließlich `Invoke-WebRequest`.

Abgesehen von PowerShell gibt es viele andere Tools, mit denen Sie Webseiten herunterladen können, z. B. `wget` und `curl` in Linux oder einfach durch Rechtsklick auf eine Webseite und "Speichern unter" in einem normalen Webbrowser.

`Invoke-WebRequest` sendet standardmäßig einen GET-Request an die angegebene URL. Es unterstützt jedoch auch POST, DELETE und andere HTTP-Methoden. Es können auch zusätzliche Header, Nutzdaten und Cookies gesendet werden.

## Siehe Auch

Weitere Informationen und Quellen können Sie auf folgenden Seiten finden:
- [Microsofts Dokumentation zu Invoke-WebRequest](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7)
- [W3Schools Tutorials zu HTML](https://www.w3schools.com/html)
- [Mozilla Developers Network (MDN) Einführung in HTTP](https://developer.mozilla.org/de/docs/Web/HTTP/Overview)