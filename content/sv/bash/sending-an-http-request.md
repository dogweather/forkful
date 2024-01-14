---
title:                "Bash: Sända en http-begäran"
simple_title:         "Sända en http-begäran"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Varför
HTTP-förfrågningar är ett vanligt sätt att interagera med webbservrar och används ofta inom programmering för att hämta data eller göra ändringar på en webbplats.

## Hur man gör det
För att skicka en HTTP-förfrågan i Bash, använder man vanligtvis kommandot `curl`. Här är ett exempel på hur man skickar en GET-förfrågan till en webbplats:

```Bash
curl https://www.example.com
```

Detta kommer att hämta hela hemsidan och skriva ut den i terminalen. Om du vill spara utdata till en fil, kan du lägga till flaggan `-o` efter kommandot och ange en filnamn att spara till. Till exempel:

```Bash
curl -o example.html https://www.example.com
```

Detta kommer att spara hemsidan till en fil med namnet "example.html". För att skicka en POST-förfrågan, kan du använda flaggan `-d` för att ange data att skicka med förfrågan. Till exempel:

```Bash
curl -d "username=John&password=12345" https://www.example.com/login
```

Detta skickar en förfrågan till webbadressen "https://www.example.com/login" med användarnamn och lösenord som data.

## Djupdykning
När du skickar en HTTP-förfrågan i Bash är det viktigt att förstå de olika komponenterna som ingår. En HTTP-förfrågan består av en verb (t.ex. GET, POST, PUT), en URL och en serie med valfria headers och data.

Verbet indikerar vilken typ av åtgärd som ska utföras på webbservern. Den vanligaste metoden är GET, som används för att hämta data från servern. Andra metoder som kan användas är POST, PUT och DELETE.

URL:en är webbadressen för servern som du vill interagera med. Det kan vara en hemsida eller en specifik sida eller resurs på webbplatsen.

Headers är frivilliga komponenter som innehåller metadata om förfrågan, såsom vilken typ av data som skickas eller vilken språkinställning som ska användas. Headers används för att ge mer information till servern om hur din förfrågan ska hanteras.

Data används för att skicka information till servern, antingen som en del av URL:en eller som en del av förfrågan. Det kan vara användarnamn och lösenord, sökparametrar eller annan data som används för att identifiera och hämta information från servern.

## Se även
- [Bash:s officiella dokumentation för `curl`](https://www.gnu.org/software/curl/)
- [En lättfattlig guide till HTTP-förfrågningar](https://developer.mozilla.org/sv/docs/Web/HTTP/Overview)
- [En introduktion till Bash programmering](https://www.digitalocean.com/community/tutorials/an-introduction-to-the-basics-of-bash-scripting)