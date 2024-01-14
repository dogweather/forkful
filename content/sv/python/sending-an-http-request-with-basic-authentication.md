---
title:                "Python: Sända en http-begäran med grundläggande autentisering"
simple_title:         "Sända en http-begäran med grundläggande autentisering"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Varför

HTTP-anslutningar är en viktig del av programmering och används för att få information från en webbserver. Genom att använda grundläggande autentisering kan användare skicka HTTP-begäran för att få åtkomst till skyddade webbsidor eller resurser.

## Så här gör du

Det första steget är att importera "urllib.request" som ger oss möjlighet att skicka HTTP-begäran. Sedan måste vi använda "urllib.request.HTTPPasswordMgrWithDefaultRealm()" för att skapa en instans av HTTPPasswordMgrWithDefaultRealm-klassen och ange URL:en som en parameter för att skapa en autentiseringsdatabas.  

Efter detta måste vi skapa en "urllib.request.HTTPBasicAuthHandler" -instans för att hantera den grundläggande autentiseringsmetoden. Nästa steg är att definiera autentiseringsinfo, inklusive användarnamn och lösenord, och sedan lägga till denna info till vår autentiseringsdatabas med hjälp av "HTTPPasswordMgr.add_password ()".

Nu är det dags att skapa vår HTTP-begäran genom att använda "urllib.request.Request()" och ange vår URL och autentiseringshandler som parametrar. Till sist utför vi vår HTTP-begäran genom att använda "urllib.request.urlopen()" och skickar begäran till servern.

## Deep Dive

HTTP-begäran med grundläggande autentisering använder ett bas64-kodat användarnamn och lösenordspar som skickas i begäran till servern. Servern kommer sedan att verifiera dessa uppgifter innan den tillåter åtkomst till den skyddade resursen.

Det är möjligt att ange olika autentiseringsmetoder och till och med skapa en anpassad autentiseringshandler för att hantera mer avancerad autentisering. Det är också viktigt att hålla autentiseringsuppgifter säkra för att undvika obehörig tillgång till skyddade resurser.

## Se också

- [Grunderna i HTTP-autentisering med Python](https://www.digitalocean.com/community/tutorials/how-to-use-basic-authentication-with-http-in-python-3)
- [Dokumentation för urllib.request i Python](https://docs.python.org/3/library/urllib.request.html)
- [Översikt över hur HTTP fungerar](https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview)