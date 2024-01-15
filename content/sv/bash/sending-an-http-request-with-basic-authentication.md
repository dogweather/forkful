---
title:                "Att skicka en http-förfrågan med grundläggande autentisering"
html_title:           "Bash: Att skicka en http-förfrågan med grundläggande autentisering"
simple_title:         "Att skicka en http-förfrågan med grundläggande autentisering"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Varför
Chansen är att om du arbetar med webbutveckling eller automatiserad scripting, har du stött på scenarier där du behöver skicka en HTTP-begäran med grundläggande autentisering. Detta är en vanlig metod för att verifiera din identitet och få åtkomst till skyddade resurser på en server.

## Så här gör du
För att skicka en HTTP-begäran med grundläggande autentisering, använder du dig av `curl`-kommandot i Bash. Här är ett grundläggande exempel för att hämta en resurs från en server som kräver grundläggande autentisering:
```Bash
curl -u användarnamn:lösenord URL
```
Sätt in dina egna användarnamn, lösenord och URL-adress i kommandot för att få åtkomst till resursen. Om autentiseringen är framgångsrik, kommer du att få utdata från resursen som svar.

För en mer omfattande guide om `curl` och HTTP-autentisering, kan du läsa dokumentationen på [Bash-sidan](https://www.gnu.org/software/bash/manual/html_node/Curl-Downloading-Options.html).

## Byte i detalj
HTTP-begäran som skickas med grundläggande autentisering innehåller en `Authorization`-rubrik som bär autentiseringsinformationen. Denna rubrik använder sig av bas64-kodning för att kapsla in användarnamnet och lösenordet. Servern kommer att avkoda denna information och verifiera ditt användarnamn och lösenord för att bestämma om du har åtkomst till de begärda resurserna.

## Se även
- [Curl dokumentation](https://curl.haxx.se/docs/httpscripting.html)
- [HTTP autentisering med Bash](https://gordonlesti.com/http-authentication-with-curl/)
- [Bas64 kodning](https://en.wikipedia.org/wiki/Base64)