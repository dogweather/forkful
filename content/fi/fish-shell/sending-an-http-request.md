---
title:                "HTTP-pyynnön lähettäminen"
html_title:           "Bash: HTTP-pyynnön lähettäminen"
simple_title:         "HTTP-pyynnön lähettäminen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

HTTP-pyyntö on tapa, jolla selain (tai muu asiakasohjelma) pyytää tietoja palvelimelta. Ohjelmoijat lähettävät näitä pyyntöjä datan saamiseksi tai lähettämiseksi palvelimille tai API:ille. 

## Näin se tapahtuu:

Fish Shell tarjoaa helpon tavan lähettää HTTP-pyyntöjä käyttäen `curl`-työkalua. Tässä on esimerkki:

```Fish Shell
curl "http://www.example.com" -o "output.html"
```

Tämä lähettää GET-pyynnön http://www.example.com:iin ja tallentaa vastauksen tiedostoon nimeltä output.html.

## Syvemmälle

HTTP-pyynnön lähettäminen kehitettiin webin varhaisessa vaiheessa, joten kaikkein ensimmäiset HTTP-pyynnöt lähetettiin tekstipohjaisesta selaimesta nimeltä Lynx. Vaihtoehtoisesti voit lähettää HTTP-pyynnön myös `wget`-työkalulla, joka toimii samankaltaisesti kuin `curl`, tai voit käyttää HTTP-kirjastoja kielissä, kuten Python tai Javascript.

HTTP-pyynnön toiminnallisuus liittyy lähinnä TCP/IP-protokollan rakenteeseen. HTTP-pyyntö luodaan, lähetetään ja vastaanotetaan tämän protokollan avulla.

## Katso myös

Jos haluat lisätietoa, katso seuraavat linkit:

1. [Fish Shell -dokumentaatio](https://fishshell.com/docs/current/index.html)
2. [Curl-käyttöohje](https://curl.haxx.se/docs/manpage.html)
3. [HTTP-protokollan työnkulku](https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview)
4. [Wget-työkalun dokumentaatio](https://www.gnu.org/software/wget/manual/wget.html)