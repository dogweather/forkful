---
title:                "Sending et http-request med grunnleggende autentisering"
html_title:           "C#: Sending et http-request med grunnleggende autentisering"
simple_title:         "Sending et http-request med grunnleggende autentisering"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
En av de vanligste måtene å kommunisere med en nettside er ved å sende en HTTP-forespørsel. Dette lar deg hente informasjon fra nettsiden og sende informasjon tilbake til den. Programmerere bruker ofte basic authentication når de sender HTTP-forespørsler for å sikre at bare autoriserte brukere har tilgang til dataene.

## Slik gjør du det:
```C#
// Setter opp en HTTP-forespørsel med basic authentication
var request = (HttpWebRequest)WebRequest.Create("nettside.com/api/data");
request.Method = "GET";

// Legger til brukerinformasjon 
request.Headers["Authorization"] = "Basic " + Convert.ToBase64String(Encoding.Default.GetBytes("brukernavn:passord"));

// Henter responsen fra serveren
var response = (HttpWebResponse)request.GetResponse();

// Leser data fra responsen
using (var stream = response.GetResponseStream())
{
    var reader = new StreamReader(stream);
    var responseText = reader.ReadToEnd();
    
    // Skriver ut dataene
    Console.WriteLine(responseText);
}
```

### Output:
```
[{ "id": 1, "navn": "John", "age": 27 }, { "id": 2, "navn": "Jane", "age": 33 }]
```

## Dykk ned:
HTTP basic authentication har eksistert siden begynnelsen av World Wide Web og er en enkel måte å autentisere brukere for tilgang til ressurser på en nettside. Alternativene til basic authentication inkluderer OAuth og API-nøkler. Implementeringen av basic authentication innebærer å inkludere brukerens autentiseringsinformasjon i headeren av en HTTP-forespørsel ved å base64-kode informasjonen.

## Se også:
- [HTTP basic authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)
- [Implementering av basic authentication i .NET](https://docs.microsoft.com/en-us/dotnet/framework/network-programming/how-to-authenticate-with-a-web-server)
- [OAuth vs Basic Authentication](https://www.guru99.com/difference-oauth-basic-authentication.html)