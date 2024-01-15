---
title:                "Ladda ner en webbsida"
html_title:           "C#: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Varför

Att ladda ner en webbplats är användbart om du vill skriva en webbprogrammeringsskript, offline-användning, eller om du vill spara en kopia av webbplatsen för framtida referens.

## Så här gör du

```C#
// Skapa en ny instans av webbclient
WebClient client = new WebClient();

// Ange webbadressen som ska laddas ner
string url = "https://www.example.com";

// Ladda ner webbsidan och spara som en textfil
client.DownloadFile(url, "sida.txt");

// Öppna textfilen och skriv ut innehållet
string innehall = File.ReadAllText("sida.txt");
Console.WriteLine(innehall);
```

Output:
```
<!DOCTYPE html>
<html>
<head>
  <title>Exempelwebbplats</title>
</head>
<body>
  <h1>Välkommen till vår webbplats!</h1>
  <p>Här kan du hitta massor av intressant information.</p>
</body>
</html>
```

## Djupdykning

Att ladda ner en webbplats innebär att du hämtar allt innehåll som finns på webbsidan, inklusive text, bilder, länkar och annan media. Du kan också använda olika metoder för att ange var du vill spara den nedladdade filen, till exempel som en textfil eller en HTML-fil. Dessutom kan du använda olika inställningar för att hantera eventuella fel eller för att ange hur länge en nedladdning ska pågå.

## Se även

- [WebClient Class](https://docs.microsoft.com/en-us/dotnet/api/system.net.webclient?view=net-5.0) - MSDN-dokumentation för C# WebClient-klassen.
- [How to Download a Web Page in C#](https://www.codeproject.com/Articles/662027/How-to-Download-a-Web-Page-in-Csharp) - En djupare guide för att ladda ner en webbplats i C#.