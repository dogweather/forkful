---
title:                "Eine Webseite herunterladen"
html_title:           "Arduino: Eine Webseite herunterladen"
simple_title:         "Eine Webseite herunterladen"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Herunterladen einer Webseite in C#

## Was & Warum?
Das Herunterladen einer Webseite ist der Prozess, bei dem der Inhalt einer Webseite abgerufen und auf dem lokalen System gespeichert wird. Programmierer machen das, um Daten für die Datenanalyse zu sammeln, Offline-Navigation zu ermöglichen oder die Website-Inhalte zu archivieren.

## Anleitung
In C# können wir die Klasse `HttpClient` aus der Namespace `System.Net.Http` verwenden, um eine Webseite herunterzuladen. 

```C#
using System;
using System.Net.Http;
using System.Threading.Tasks;

class Program {
    private static readonly HttpClient client = new HttpClient();

    static void Main(string[] args){
        DownloadPageAsync();
        Console.ReadLine();
    }

    private static async Task DownloadPageAsync() {
        var response = await client.GetAsync("http://example.com");
        string content = await response.Content.ReadAsStringAsync();
        Console.WriteLine(content);
    }
}
```

Wenn wir das Programm ausführen, erhalten wir die HTML-Ausgabe der angegebenen Webseite auf der Konsole.

## Tiefere Einsichten
Historisch gesehen benutzten wir `WebClient` für solche Aufgaben, aber `HttpClient` ist die modernere und flexiblere Alternative. Das Singleton-Muster ist auch in der obigen Implementierung zu sehen, das hilft, unnötige Instanzen und damit verbundene kostspielige Ressourcenzuweisungen zu vermeiden.

Eine Alternative zum Herunterladen von Webseiten könnte die Nutzung von APIs sein, wenn diese verfügbar sind. APIs liefern oft datenintensivere und strukturierte Daten.

Beim Herunterladen von Webseiten ist zu beachten, dass der Zugriff auf bestimmte Seiten durch Nutzungsbedingungen geregelt sein kann. Daher ist es wichtig, alle relevanten rechtlichen Aspekte zu berücksichtigen.

## Siehe auch
Für weitere Informationen, siehe die offizielle Dokumentation:

- [`HttpClient` Klasse](https://docs.microsoft.com/de-de/dotnet/api/system.net.http.httpclient?view=net-5.0)
- [Asynchrone Programmierung](https://docs.microsoft.com/de-de/dotnet/csharp/programming-guide/concepts/async/)
- [`WebClient` Klasse](https://docs.microsoft.com/de-de/dotnet/api/system.net.webclient?view=net-5.0)

Bitte beachte, dass es wichtig ist, sich mit den Nutzungsbedingungen der Webseite vertraut zu machen, bevor du versuchst, ihre Inhalte herunterzuladen.