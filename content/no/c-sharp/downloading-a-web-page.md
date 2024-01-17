---
title:                "Nedlasting av en nettside"
html_title:           "C#: Nedlasting av en nettside"
simple_title:         "Nedlasting av en nettside"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Når du laster ned en nettside, henter du innholdet på siden og lagrer det på datamaskinen din. Dette er nyttig for programmerere fordi det lar dem manipulere og behandle dataene fra nettsiden på en enkel måte, for eksempel å hente ut spesifikke informasjon eller analysere dataene.

## Slik gjør du:

```C#
using System;
using System.Net;

class Program
{
    static void Main()
    {
        //Oppretter en Webclient
        WebClient client = new WebClient();

        //Laster ned innholdet til en nettside og lagrer det i en streng
        string webpage = client.DownloadString("https://www.example.com");

        Console.WriteLine(webpage); //Skriver ut nettsiden i konsollen
    }
}
```

Output:

```html
<!doctype html>
<html>
<head>
<title>Example Domain</title>
<meta charset="utf-8" />
<meta http-equiv="Content-type" content="text/html; charset=utf-8" />
<meta name="viewport" content="width=device-width, initial-scale=1" />
<style type="text/css">
/* CSS code her... */
</style>
</head>

<body>
<div>
    <h1>Example Domain</h1>
    <p>This domain is established to be used for illustrative examples in documents. You may use this
    domain in examples without prior coordination or asking for permission.</p>
    <p><a href="https://www.iana.org/domains/example">More information...</a></p>
</div>
</body>
</html>
```

## Dypdykk:

Weblesere gjør også nedlasting av nettsider for å vise dem til brukeren. Alternativt kan man bruke en annen metode for å laste ned og behandle nettsidedata, for eksempel HTTP-forespørsler eller tredjepartsbiblioteker. Når du laster ned en nettside i C#, bruker du vanligvis en ```WebClient``` eller ```HttpClient``` klasse. Disse klassene har metoder for å sende HTTP-forespørsler og motta responsen.

## Se også:

- Microsoft sin [dokumentasjon](https://docs.microsoft.com/en-us/dotnet/api/system.net.webclient?view=net-5.0) for WebClient-klassen.
- En [tutorial](https://www.tutorialspoint.com/csharp/csharp_web_client.htm) fra Tutorialspoint om hvordan du bruker WebClient-klassen i C#.
- Alternativet til WebClient, HttpClient-klassen, som er mer fleksibel og skaleres bedre, men er også mer kompleks å bruke.