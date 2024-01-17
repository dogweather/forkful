---
title:                "Lähetetään http-pyyntö"
html_title:           "Bash: Lähetetään http-pyyntö"
simple_title:         "Lähetetään http-pyyntö"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

Sending an HTTP request, or "pinging" a website or server, is when a computer sends a message to a specific address and waits for a response. This is a common practice for programmers to check if a website or server is up and running, or to retrieve data from the website or server.

## Miten:

Bashilla voit lähettää HTTP-pyyntöjä helposti käyttämällä `curl` -komentoa. Tässä on esimerkki kuinka lähetät HTTP GET -pyynnön ja tulostat vastauksen:

```bash
curl https://www.examplewebsite.com
```

Tämä tulostaa HTML-koodin `www.examplewebsite.com` sivulta.

## Syväsyventyminen:

HTTP-protokollalla on pitkä historia, joka alkoi vuonna 1989. Nykyään on olemassa myös muita tapoja lähettää pyyntöjä, kuten käyttämällä REST-apia. Bash ei ole ainoa vaihtoehto, voit myös käyttää muita ohjelmointikieliä, kuten Pythonia, lähettämään HTTP-pyyntöjä. 

Bashilla on myös mahdollista lähettää muita HTTP-metodeja, kuten POST ja PUT, käyttämällä `curl` -komennon eri vaihtoehtoja. Voit myös lisätä otsikoita ja tietoja pyyntöön käyttämällä `-H` ja `-d` vaihtoehtoja.

## Katso myös:

- Virallinen Bash-sivusto https://www.gnu.org/software/bash/
- `curl` komentorivin ohjeet https://curl.haxx.se/docs/manpage.html