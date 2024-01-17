---
title:                "Sända en HTTP-begäran med grundläggande autentisering"
html_title:           "Java: Sända en HTTP-begäran med grundläggande autentisering"
simple_title:         "Sända en HTTP-begäran med grundläggande autentisering"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad & Varför?
När en programmerare skickar en HTTP-förfrågan med grundläggande autentisering, betyder det att de inkluderar ett inloggningsnamn och lösenord i förfrågningshuvudet. Detta används för att säkert identifiera användaren som gör förfrågan och åtkomst till skyddad data. Det är viktigt att använda grundläggande autentisering för att skydda känslig information och säkerställa att endast auktoriserade användare har tillgång till den.

## Så här gör du:
Här är ett enkelt kodexempel som visar hur du skickar en HTTP-förfrågan med grundläggande autentisering i Java:

```
URL url = new URL("http://www.example.com/api/endpoint");
HttpURLConnection connection = (HttpURLConnection) url.openConnection();

// Lägg till inloggningsuppgifter i förfrågningshuvudet
String login = "användarnamn";
String password = "lösenord";
String auth = login + ":" + password;
byte[] encodedAuth = Base64.getEncoder().encode(auth.getBytes());
String authHeaderValue = "Basic " + new String(encodedAuth);
connection.setRequestProperty("Authorization", authHeaderValue);

// Skicka förfrågan och läs svarskoden
int responseCode = connection.getResponseCode();
System.out.println("Svarskod: " + responseCode);
```

Förväntad utmatning:

```
Svarskod: 200
```

Detta exempel visar hur man skickar en GET-förfrågan med grundläggande autentisering. Det är också möjligt att använda det här tillvägagångssättet för att skicka andra typer av förfrågningar som POST, PUT, DELETE, etc.

## Djupdykning:
HTTP-grundläggande autentisering har funnits sedan det första HTTP-protokollets utveckling på 1990-talet. Det är den enklaste formen av autentisering och stöds av de flesta webbservrar och klienter. Det finns dock alternativ till grundläggande autentisering, såsom Digest Authentication och OAuth, som erbjuder bättre säkerhetsnivåer.

För att implementera grundläggande autentisering i Java, kan du använda klassen "HttpURLConnection" som visas i exemplet ovan eller använda ett HTTP-bibliotek som Apache HttpClient eller OkHttp.

## Se även:
- [HttpURLConnection Class](https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html)
- [Basic Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)