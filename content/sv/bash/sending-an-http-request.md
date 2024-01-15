---
title:                "Skicka en http-begäran"
html_title:           "Bash: Skicka en http-begäran"
simple_title:         "Skicka en http-begäran"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Varför

För att interagera med webbsidor och tjänster är det ofta nödvändigt att skicka HTTP-förfrågningar. Detta kan vara för att hämta information, skicka data eller utföra olika handlingar.

## Hur man gör det

För att skicka en HTTP-förfrågan i Bash finns det ett inbyggt kommando som heter "curl". Detta kommando kan ange URL-adressen som förfrågan ska skickas till och eventuella parametrar som behövs. Exempelvis:

```Bash
curl https://www.example.com
```

Detta kommer att skicka en GET-förfrågan till webbplatsen www.example.com och returnera svaret, som i detta fall skulle vara HTML-koden för webbsidan. Om du vill skicka en POST-förfrågan med data, kan du använda flaggan "-d" och ange dina data i citattecken. Exempelvis:

```Bash
curl -d 'username=johndoe&password=123456' https://www.example.com/login
```

I detta exempel kommer en POST-förfrågan att skickas till /login-sidan med användarnamn och lösenord som dataparametrar.

## Fördjupning

Curl-kommandot har många fler alternativ och funktioner än de som nämns ovan. Du kan använda flaggor som "-H" för att ange HTTP-huvuden, "-X" för att ange en annan HTTP-metod än den som standard används, och "-o" för att ladda ner resultatet till en fil istället för att skriva ut det i terminalen. Du kan också använda curl för att skicka förfrågningar till lokala servrar eller för att autentisera med olika metoder. Det finns massor av bra resurser på nätet för att lära sig mer om curl och dess alla användningsområden.

## Se även

- [Curl man-sidan](https://linux.die.net/man/1/curl)
- [Curl-guide på DigitalOcean](https://www.digitalocean.com/community/tutorials/how-to-use-curl-to-download-files-from-the-command-line)
- [Curl för idioter](http://www.geekality.net/tag/curl-for-dummies/)