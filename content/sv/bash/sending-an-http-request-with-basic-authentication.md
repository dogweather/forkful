---
title:                "Skicka en http-begäran med grundläggande autentisering"
html_title:           "Elixir: Skicka en http-begäran med grundläggande autentisering"
simple_title:         "Skicka en http-begäran med grundläggande autentisering"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Skicka HTTP-begäran med Grunden Tillbehör: En guide till Bash-programmering

## Vad & Varför?
Att skicka en HTTP-begäran med grundläggande autentisering innebär att programmeraren skickar en förfrågan till en server till internet med bevis på identitet. Vi gör detta för att få tillgång till skyddad data eller utföra åtgärder på servern.

## Hur man:
Använd `curl` -kommandot för att skicka HTTP-begäran med grundläggande autentisering.

```Bash
# Skicka en begäran till exempel.com
# Användare: användarnamn, Lösenord: lösenord
curl -u användarnamn:lösenord https://exempel.com
```
När du kör detta kommando, kommer du att se något liknande:
```Bash
<!doctype html>
<html>
<head>
    <title>Exempel Start Page</title>
</head>
<body>
    <h1>Welcome, användarnamn!</h1>
</body>
</html>
```
## Fördjupning 
Historiskt sett började basautentisering användas i webb-HTTP/1.0-protokollet och fortsätter att användas i HTTP/1.1. Summit ägde rum 2019 i som introducerade HTTP/2.

Alternativa sätt för autentisering inkluderar Digest Access Authentication, OAuth, OpenID Connect, och mer. 

En viktig punkt att notera vid implementering av grundläggande autentisering är att användarinformation (användarnamn och lösenord) inte är krypterad, men är kodad med bas64 - vilket är lätt att avkoda. Använd alltid HTTPS för att skydda användaruppgifter på vägen.

## Se också
1. [curl man sidor](https://curl.haxx.se/docs/manpage.html)
2. [HTTP/1.1 autentiseringsspecifikation](https://tools.ietf.org/html/rfc2617)
3. [Säker transport med HTTPS](https://www.eff.org/pages/how-deploy-https-correctly)