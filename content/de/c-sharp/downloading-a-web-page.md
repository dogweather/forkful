---
title:                "Herunterladen einer Webseite"
html_title:           "C#: Herunterladen einer Webseite"
simple_title:         "Herunterladen einer Webseite"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Warum

Wenn du in der Welt der Programmierung unterwegs bist, ist es unvermeidbar, dass du irgendwann auf das Thema Webseiten-Downloads stößt. Ob du nun Daten für eine Analyse benötigst oder einfach nur neugierig bist, es gibt viele Gründe, warum es wichtig ist, eine Webseite herunterzuladen. Glücklicherweise ist dies mit C# ein einfacher Prozess.

## Wie es geht

Um eine Webseite in C# herunterzuladen, benötigst du die `System.Net`-Bibliothek. Du kannst dann eine Instanz der `WebClient`-Klasse erstellen und die `DownloadFile`-Methode verwenden, um die Seite herunterzuladen. Hier ist ein Beispielcode:

```C#
using System;
using System.Net;

var client = new WebClient();
client.DownloadFile("https://example.com", "example.html");
```

Dieser Code erstellt einen `WebClient` mit dem Namen `client` und verwendet dann die `DownloadFile`-Methode, um die Seite von `https://example.com` herunterzuladen und als `example.html` zu speichern.

Du kannst auch die `DownloadString`-Methode verwenden, um den HTML-Code einer Seite als Zeichenfolge herunterzuladen. Hier ist ein Beispiel:

```C#
using System;
using System.Net;

var client = new WebClient();
var html = client.DownloadString("https://example.com");
```

Dieser Code erstellt wieder einen `WebClient` mit dem Namen `client`, aber dieses Mal wird die `DownloadString`-Methode verwendet, um den HTML-Code von `https://example.com` zu erhalten und in der Variablen `html` zu speichern.

## Tiefere Einblicke

Während die `WebClient`-Klasse äußerst hilfreich ist, gibt es auch andere Möglichkeiten, Webseiten in C# herunterzuladen, wie zum Beispiel die Verwendung von `HttpWebRequest` und `HttpWebResponse`.

Eine weitere wichtige Überlegung ist, wie du mit Problemen wie Zeitüberschreitungen und Fehlercodes umgehst. Du kannst dies durch das Setzen von Zeitlimits oder das Überprüfen von HTTP-Antworten tun.

Es gibt auch weitere Optionen für das Anpassen des Prozesses, wie das Herunterladen der Seite in Teilen oder das Hinzufügen von Cookies.

## Siehe auch

- [Dokumentation von `WebClient`](https://docs.microsoft.com/en-us/dotnet/api/system.net.webclient?view=netcore-3.1)
- [Dokumentation von `HttpWebRequest` und `HttpWebResponse`](https://docs.microsoft.com/en-us/dotnet/api/system.net.httpwebrequest?view=netcore-3.1)
- [Beispielprojekt für Webseiten-Downloads in C#](https://github.com/example/webpage-downloader-csharp)