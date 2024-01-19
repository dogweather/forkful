---
title:                "Skicka en http-begäran med grundläggande autentisering"
html_title:           "Elixir: Skicka en http-begäran med grundläggande autentisering"
simple_title:         "Skicka en http-begäran med grundläggande autentisering"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Skicka HTTP-förfrågan med grundläggande autentisering i Lua.

## Vad och Varför?
Att skicka en HTTP-förfrågan med grundläggande autentisering innebär att vi sänder en säker anslutningsbegäran till en server. Detta görs av programmerare för att tillhandahålla en manipulerbar och säker tillgång till webbaserade tjänster.

## Hur såhär:
Vi kan använda modulen `LuaSocket` och `luasec` för HTTP-anrop. Men här kommer ett alternativt och enkelt sätt att använda `os.execute` med `curl`.

```Lua
os.execute('curl -u användarnamn:lösenord http://api.webbplats.com/resurs')
```
Var noga med att byta ut `användarnamn` och `lösenord` med dina riktiga uppgifter. `curl` skickar nu en HTTP GET-förfrågan till webbadressen med grundläggande autentisering.

Skickar du denna förfrågan kan du få ett svar som liknar något nedan:

```Lua
{ 'data': { 'key1': 'värde1', 'key2': 'värde2' } }
```
## Djup dykning
Grundläggande autentisering är en standardprocess för att verifiera användare av webbapplikationer. Innan Lua, Curl, och liknande verktyg användes, måste programmerare ställa in och hantera egna metoder för autentisering.

Det finns alternativ till grundläggande autentisering, som OAuth, som ger mer komplexa autentiseringsmöjligheter men också mer kontroll.

När det gäller användning av `os.execute` med `curl`, det skickar och tar emot data genom HTTP mellan klient och server, med `användarnamn` och `lösenord` kodat i Base64. HTTP-header för grundläggande autentisering tittar på detta:

```Lua
Authorization: Basic base64Encode(användarnamn:lösenord)
```
## Se även
För mer information om 

- [Grundläggande autentisering](https://en.wikipedia.org/wiki/Basic_access_authentication)
- [Lua och Curl](https://stackoverflow.com/questions/63810057/how-do-i-send-a-get-request-in-lua)
- [Alternativ till grundläggande autentisering](https://jwt.io/introduction/)
- [Mer om HTTP header](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Authorization) 

Bemästra hur man skickar en HTTP-förfrågan med grundläggande autentisering i Lua skapar ett starkt grund för att bygga webbaserade applikationer.