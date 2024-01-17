---
title:                "Skicka en http-begäran med grundläggande autentisering"
html_title:           "Fish Shell: Skicka en http-begäran med grundläggande autentisering"
simple_title:         "Skicka en http-begäran med grundläggande autentisering"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skicka en HTTP-begäran med grundläggande autentisering är en metod som används av programmerare för att säkert kommunicera med webbservern. Det innebär att en användare måste ange ett användarnamn och lösenord för att få tillgång till resurser på webbplatsen.

## Hur gör man:
Fish Shell har en inbyggd funktion för att skicka en HTTP-begäran med grundläggande autentisering. Här är ett exempel på hur det kan se ut:

```
fish -c "curl -u användarnamn:lösenord http://www.example.com"
```

Detta kommer att skicka en GET-begäran med grundläggande autentisering till www.example.com och returnera resultatet i terminalen. Om du vill skicka en POST-begäran, lägg till "-X POST" efter "curl". Du kan även lägga till ytterligare parametrar efter URL:en, till exempel "-d" för att skicka data.

## Djupdykning:
Att skicka en HTTP-begäran med grundläggande autentisering har funnits sedan HTTP-protokollet utvecklades för att säkert överföra data över internet. I Fish Shell finns det även andra sätt att skicka HTTP-begäran, som "curl" eller "wget", men Fish Shell-funktionen gör det enkelt att skicka begäran utan att behöva använda ett annat program.

## Se även:
- [Fish Shell](https://fishshell.com/)
- [Grundläggande autentisering på Wikipedia](https://sv.wikipedia.org/wiki/Grundl%C3%A4ggande_autentisering)
- [Curl dokumentation](https://curl.haxx.se/docs/)