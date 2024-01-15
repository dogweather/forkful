---
title:                "Sända en http-förfrågan"
html_title:           "C#: Sända en http-förfrågan"
simple_title:         "Sända en http-förfrågan"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Varför

Att skicka en HTTP-begäran kan verka som en grundläggande uppgift, men det är en viktig del av nätverksprogrammering. Genom att förstå hur man skickar en HTTP-begäran kan du integrera dina applikationer med externa API:er och webbtjänster.

## Hur man gör det

```C#
// Skapa en HTTP-begäran
var request = (HttpWebRequest)WebRequest.Create("http://www.example.com");

// Välj önskat HTTP-verb
request.Method = "GET";

// Skicka förfrågan och ta emot svar
var response = (HttpWebResponse)request.GetResponse();

// Läs svarsinnehållet
using (var streamReader = new StreamReader(response.GetResponseStream()))
{
    var result = streamReader.ReadToEnd();
}

// Stäng anslutningen
response.Close();
```

## Djupdykning

När du skickar en HTTP-begäran finns det flera aspekter som är viktiga att ta hänsyn till. Du behöver ange rätt HTTP-verb, välja rätt kodning för data och hantera eventuella fel eller avbrott i förfrågan. Det finns även olika typer av begäran som kan skickas, till exempel GET, POST, PUT och DELETE. Genom att förstå dessa detaljer kan du skriva mer robust och effektiv kod för att kommunicera över nätverket.

## Se även

- [HTTP-begäran i C#](https://docs.microsoft.com/sv-se/dotnet/api/system.net.httpwebrequest?view=netcore-3.1)
- [Skillnad mellan GET och POST i HTTP](https://www.w3schools.com/tags/ref_httpmethods.asp)
- [Förståelse för HTTP-statuskoder](https://developer.mozilla.org/sv-SE/docs/Web/HTTP/Status#Vanliga_statuskoder)