---
title:                "Fish Shell: Skicka en http-förfrågan med grundläggande autentisering"
simple_title:         "Skicka en http-förfrågan med grundläggande autentisering"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Varför

Att skicka en HTTP-begäran med grundläggande autentisering kan vara en nödvändig del av många Fish Shell-program. Genom att använda denna metod kan du säkert skicka användarnamn och lösenord till en server för autentiseringsändamål.

## Så här gör du

Det första steget är att definiera din begäran med rätt URL och metod. I detta exempel använder vi kommandot *curl* för att skicka en GET-begäran till en server:

```Fish Shell
curl example.com/api -X GET
```

Nu måste vi lägga till den grundläggande autentiseringsinformationen. Detta kan göras genom att använda flaggan *-u* följt av ditt användarnamn och lösenord:

```Fish Shell
curl example.com/api -X GET -u username:password
```

Ett annat sätt att göra detta är att använda flaggan *--basic*:

```Fish Shell
curl example.com/api -X GET --basic -u username:password
```

Om autentiseringen lyckas, kommer du att få ett svar från servern med statuskoden 200. Om autentiseringen misslyckas får du istället en statuskod på 401. Se till att inkludera autentiseringsinformationen i varje begäran till servern för att säkerställa en korrekt autentiseringsprocess.

## Deep Dive

Vad händer egentligen när vi skickar en HTTP-begäran med grundläggande autentisering? När vi inkluderar autentiseringsinformationen i vår begäran, skickas en särskild HTTP-header, kallad *Authorization*, till servern. Denna header innehåller bas64-kodad information som visar vårt användarnamn och lösenord i formatet "Användarnamn:Lösenord". Servern kan då dekoda denna information och autentisera oss innan den behandlar begäran.

Det är viktigt att komma ihåg att grundläggande autentisering inte krypterar någon av dina autentiseringsuppgifter. Det enda skydd som erbjuds är baserad på att informationen är kodad i bas64-format. För att säkerställa en riktigt säker autentiseringsprocess bör du istället överväga att använda andra autentiseringsmetoder som OAuth eller JWT.

## Se även

- [Curl dokumentation](https://curl.se/docs/)
- [HTTP-headers och autentisering](https://www.w3.org/Protocols/HTTP/1.0/spec.html#Authentication)