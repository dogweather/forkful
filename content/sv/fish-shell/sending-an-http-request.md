---
title:                "Skicka en http-förfrågan"
html_title:           "Javascript: Skicka en http-förfrågan"
simple_title:         "Skicka en http-förfrågan"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att skicka en HTTP-begäran är helt enkelt att begära data från en annan server via HTTP-protokollet. Programmerare gör detta för att interagera med webbtjänster och API:er för att hämta, skapa, uppdatera eller ta bort data.

## Hur Man:

För att skicka en HTTP-begäran inom Fish Shell kan vi använda 'curl' kommandot.

```Fish Shell
# För en GET-begäran
curl http://example.com

# För en POST-begäran
curl -X POST -d "data to send" http://example.com
```

Exempel på output:

```Fish Shell
<!doctype html>
<html>
...
</html>
```

## Djupdykning

Historiskt sett togs HTTP (Hypertext Transfer Protocol) fram som ett sätt för webbläsare att hämta webbsidor från servrar. Med uppkomsten av webbtjänster och API:er används det nu mer generellt för att skicka data mellan klienter och servrar.

Alternativen till HTTP inkluderar andra kommunikationsprotokoll som FTP eller SMTP, men HTTP är mest omfattande för webbinteraktioner och är därmed vald för exempel här.

När vi skickar en HTTP-begäran med Fish Shell och curl, händer flera saker i bakgrunden. Curl skapar en anslutning till servern, skickar begäran, och tar emot serverns svar. Detta innefattar både data och metadata, som statuskoder och headers.

## Se Även

För mer djupgående information om HTTP och curl:

- [Mozilla HTTP Guide](https://developer.mozilla.org/en-US/docs/Web/HTTP)
- [Curl Manual](https://curl.se/docs/manpage.html)