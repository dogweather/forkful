---
title:                "Sända en http-begäran med grundläggande autentisering"
html_title:           "C++: Sända en http-begäran med grundläggande autentisering"
simple_title:         "Sända en http-begäran med grundläggande autentisering"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad & Varför?
När programmerare skickar en HTTP-begäran med grundläggande autentisering använder de sig av en användarnamns- och lösenordskombination för att autentisera sig själva och få åtkomst till en resurs på en server. Det kan vara användbart för att skydda känslig information eller för att bara tillåta auktoriserade användare att komma åt en viss resurs på en server.

## Så här gör du:
```C++
//Skapa en grundläggande autentiseringssträng
std::string basicAuth = "användarnamn:lösenord";

//Kod för att skicka en HTTP GET-begäran med grundläggande autentisering
//Först skapa en HTTP-klient och ställa in URL:en för begäran
http::Client client;
client.set_url("https://www.example.com");

//Ange grundläggande autentisering i begäran
client.set_basic_auth(basicAuth);

//Utför sedan begäran och hämta svaret
http::Response response = client.get();

//Skriv ut svaret som en sträng
std::cout << response.body();

// Om begäran lyckas bör svaret innehålla det efterfrågade innehållet

```

## Djupdykning:
HTTP-begäran med grundläggande autentisering är en äldre autentiseringsmetod som används mindre och mindre idag till förmån för mer säkra metoder som OAuth. Det är dock fortfarande en enkel och tillförlitlig lösning för grundläggande autentisering av webbtjänster. Istället för att skicka inloggningsuppgifterna i klartext skickas en kodad version, vilket gör det svårare för obehöriga att få tillgång till lösenordet.

## Se även:
- [HTTP-begäran med grundläggande autentisering - Dokumentation för HTTP-biblioteket i C++](https://www.httplib.net/message/documentation.html#Client-HTTP-basic-authentication)
- [HTTP-begäran med grundläggande autentisering - Wikipedia](https://en.wikipedia.org/wiki/Basic_access_authentication)