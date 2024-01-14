---
title:                "C#: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Varför man ska ladda ner en webbsida
När man surfar på internet stöter man ofta på intressanta webbsidor som man vill spara för senare läsning. Genom att ladda ner webbsidan kan man spara den lokalt på sin dator och enkelt öppna den utan internetanslutning eller risk för att sidan försvinner.

## Hur man laddar ner en webbsida med C#
Att ladda ner en webbsida med C# är en enkel process som kan göras med hjälp av .NET framework och en kraftfull klass som heter WebClient. Här är ett exempel på hur man laddar ner en webbsida och skriver ut dess innehåll i konsolen:

```C#
using System;
using System.Net;

class Program
{
    static void Main(string[] args)
    {
        // Skapa en instans av WebClient
        WebClient client = new WebClient();

        // Ladda ner innehållet från en webbsida
        string webpage = client.DownloadString("https://www.mittexempelwebbsida.se");

        // Skriv ut innehållet i konsolen
        Console.WriteLine(webpage);
    }
}
```

Output:
```
<!DOCTYPE html>
<html>
<head>
    <title>Min Exempelwebbsida</title>
</head>
<body>
    <h1>Välkommen till min exempelsida</h1>
    <p>Här kan du hitta massor av intressanta artiklar och information.</p>
</body>
</html>
```

## Fördjupning i att ladda ner en webbsida
Genom att använda WebClient-klassen har man också möjlighet att ladda ner och spara filer från webbsidan, till exempel bilder eller dokument. Det går också att skicka anpassade HTTP-begäranden för att hämta specifika delar av en webbsida eller få den i ett annat format.

Det finns också andra sätt att ladda ner en webbsida med C#, som att använda HTTP-klient och asynkrona operationer för att öka prestandan. Det är också viktigt att ta hänsyn till eventuella tillstånd från webbsidan, såsom godkännande av användaravtalet innan man laddar ner innehållet.

## Se även
- [WebClient-klassen på Microsoft Docs](https://docs.microsoft.com/sv-se/dotnet/api/system.net.webclient?view=net-5.0)
- [HTTP-klient på Microsoft Docs](https://docs.microsoft.com/sv-se/dotnet/api/system.net.http.httpclient?view=net-5.0)
- [Asynkron programmering i C# på Clean Code](https://cleancoders.com/episode/clean-code-episode-10/show)