---
title:                "Webseite herunterladen"
html_title:           "C#: Webseite herunterladen"
simple_title:         "Webseite herunterladen"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Was & Warum?
Webseiten herunterzuladen bedeutet, den Inhalt einer Internetseite auf deinen Computer zu übertragen. Programmierer machen dies, um den Quellcode einer Seite zu analysieren, Daten abzurufen oder Inhalte auf einer Webseite zu bearbeiten.

## Wie geht's?
Das Herunterladen einer Webseite in C# ist relativ einfach. Du kannst die `HttpClient` Klasse verwenden, um eine Verbindung mit der Webseite herzustellen und dann die `GetStringAsync()` Methode aufrufen, um den Inhalt der Seite als String zu erhalten.
```C#
using System.Net.Http;

var client = new HttpClient();
var response = await client.GetStringAsync("https://www.beispielwebseite.com");
Console.WriteLine(response);
```
Dieser Code wird den gesamten HTML-Code der Webseite auf der Konsole ausgeben.

Du kannst auch den `HttpWebRequest` und `HttpWebResponse` verwenden, um eine Verbindung herzustellen und den Inhalt der Seite als Stream zu erhalten.
```C#
var request = (HttpWebRequest)WebRequest.Create("https://www.beispielwebseite.com");
var response = (HttpWebResponse)request.GetResponse();
var stream = response.GetResponseStream();

StreamReader reader = new StreamReader(stream);
Console.WriteLine(reader.ReadToEnd());
```
Dieser Code wird ebenfalls den gesamten HTML-Code auf der Konsole ausgeben, funktioniert aber etwas anders als der vorherige Ansatz.

## Tiefer tauchen
Das Herunterladen von Webseiten hat in der Vergangenheit oft illegale Zwecke gehabt, wie das Spiegeln von Inhalten oder das Umgehen von Zugriffsbeschränkungen. Daher kann es sein, dass du bei deinem Versuch, eine Webseite herunterzuladen, auf eine Anti-Scraping-Technik stößt, die das Herunterladen von Inhalten erschwert oder sogar blockiert.

Eine Alternative zum direkten Herunterladen einer Webseite ist die Verwendung von Web Scraping Frameworks wie Scrapy oder BeautifulSoup. Diese Frameworks bieten eine Vielzahl von Funktionen, die es dir ermöglichen, spezifische Teile von Webseiten zu extrahieren oder ganze Webseiten herunterzuladen.

Um Webseiten in deinem Code herunterzuladen, solltest du dich auch mit den Themen Cookies, Authentifizierung und HTTP-Header vertraut machen. Diese spielen oft eine wichtige Rolle beim Zugriff auf Webseiten und können bei fehlerhafter Implementierung zu Problemen führen.

Beim Herunterladen von Webseiten solltest du auch darauf achten, dass du nicht gegen die Nutzungsbedingungen der Seite verstößt. Manche Webseiten verbieten das Scrapen ihrer Inhalte und können rechtliche Schritte einleiten, wenn du trotzdem ihre Inhalte herunterlädst.

## Siehe auch
Für weitere Informationen zum Herunterladen von Webseiten in C# kannst du folgende Quellen besuchen:

- [MSDN - HttpClient Klasse](https://docs.microsoft.com/de-de/dotnet/api/system.net.http.httpclient)
- [Codeburst - A Beginner's Guide to Web Scraping in C#](https://codeburst.io/a-beginners-guide-to-web-scraping-in-c-40ec4770e125)
- [GeeksforGeeks - Web Scraping Using BeautifulSoup in C#](https://www.geeksforgeeks.org/web-scraping-using-beautifulsoup-in-c-sharp/)