---
title:                "Sända en http-begäran med grundläggande autentisering"
html_title:           "C#: Sända en http-begäran med grundläggande autentisering"
simple_title:         "Sända en http-begäran med grundläggande autentisering"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skicka en HTTP-förfrågan med grundläggande autentisering innebär att man skickar ett användarnamn och lösenord för att få tillgång till ett webbgränssnitt eller en API-tjänst. Program utvecklare använder det för att säkra åtkomsten till sina applikationer och skydda känslig information från obehöriga.

## Så här:
```C#
// Skapa en klient för HTTP-förfrågan
var client = new HttpClient();

// Ange autentisering att använda basic authentication
var auth = new AuthenticationHeaderValue("Basic", Convert.ToBase64String(Encoding.ASCII.GetBytes("användarnamn:lösenord")));

// Ange användarnamn och lösenord för HTTP-förfrågan
client.DefaultRequestHeaders.Authorization = auth;

// Skicka en GET-förfrågan till en API-tjänst
var response = await client.GetAsync("https://exempel.com/api/resurs");

// Hämta svarsinnehållet
var responseContent = await response.Content.ReadAsStringAsync();

// Skriv ut svaret
Console.WriteLine(responseContent);
```

**Exempelutgång:**

```
{ "status": "success", "data": { "resurs": "information" } }
```

## Djupdykning:
Under 1990-talet skapades grundläggande autentisering som en enkel form av autentisering för HTTP. Det är enkelt att implementera och kräver bara enkla ändringar i HTTP-förfrågan. Alternativ till grundläggande autentisering inkluderar bearer-token-autentisering och OAuth.

## Se även:
- [Microsofts dokumentation för HTTP-basautentisering](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient?view=netcore-3.1)
- [Huvudmetoder för HTTP-åtkomstkontroll](https://www.w3schools.com/tags/ref_httpmethods.asp)