---
title:                "Sända en http-begäran med grundläggande autentisering"
html_title:           "Fish Shell: Sända en http-begäran med grundläggande autentisering"
simple_title:         "Sända en http-begäran med grundläggande autentisering"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Varför
Det finns många tillfällen där du behöver skicka en HTTP-begäran med grundlegitimering, till exempel när du kommunicerar med en API-tjänst som kräver autentisering. Genom att lära dig hur man gör det i Fish Shell kan du enkelt automatisera uppgifter som kräver kommunikation med externa servrar.

## Hur man gör
För att kunna skicka en HTTP-begäran med grundlegitimering i Fish Shell behöver du använda kommandot "curl". Curl är ett verktyg som används för att skicka och ta emot data från en server. Här är ett exempel på hur du kan skicka en GET-begäran med grundlegitimering och spara svaret i en textfil:

```
fish shell 
curl -u username:password -o response.txt https://www.example.com/api/endpoint
```

I det här exemplet ersätter du "username" och "password" med dina faktiska autentiseringsuppgifter och "https://www.example.com/api/endpoint" med den URL du vill skicka begäran till. Detta kommer att skapa en textfil med namnet "response.txt" som innehåller svaret från servern.

## Djupdykning
För att förstå hur det här kommandot fungerar behöver vi bryta ner det och förklara varje del. "Curl" är kommandonamnet som startar programvaran som används för att skicka begäran. "-u" är en flagga som indikerar att du ska använda grundläggande autentisering. "Username" och "password" är de autentiseringsuppgifter som krävs. "-o" är en flagga som talar om att du vill spara utdata från servern i en fil, och "response.txt" är namnet på den filen. Slutligen är "https://www.example.com/api/endpoint" den URL du skickar begäran till.

## Se även
- [Fish Shell](https://fishshell.com/)
- [Curl dokumentation](https://curl.se/docs/manual.html)
- [HTTP-begäran med grundläggande autentisering](https://www.twilio.com/blog/2017/02/an-easy-way-to-read-and-write-to-a-google-spreadsheet-in-python.html)