---
title:                "Bash: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Miksi?

Monet ihmiset haluavat ladata verkkosivuja eri tarkoituksia varten, kuten offline-lukemista tai tietojen tallentamista. Bash-skripti on hyvä työkalu tämän tehtävän suorittamiseen, koska se tarjoaa helpon ja nopean tavan ladata verkkosivuja komentoriviltä.

## Miten tehdä?

Ladataksesi verkkosivun käyttäen Bashia, sinun täytyy tietää muutamia tärkeitä Bash-komentoja ja niiden käyttötarkoituksia. Alla on esimerkki miten voit ladata tyhjän sivun ja tulostaa sen sisällön terminaaliin:

```Bash
#!/bin/bash
# Lataa sivu ja tulosta sen sisältö
curl http://www.example.com
```

Tämä yksinkertainen Bash-skripti käyttää `curl` -komentoa ladatakseen verkkosivun URL:sta, joka annetaan komentorivillä. Voit myös tallentaa sivun tiedostoon tai jopa lähettää sen eteenpäin toiseen URL:een. Esimerkiksi:

```Bash
#!/bin/bash
# Lataa sivu ja tallenna se tiedostoon
curl http://www.example.com -o example.html

# Lähetä ladattu sivu toiseen URL:een
curl -T example.html http://www.example2.com
```

Voit myös käyttää `wget` -komentoa lataamaan sivuja Bash-skriptistä. Se toimii hyvin samalla tavalla kuin `curl`, mutta siinä on hieman erilaiset vaihtoehdot ja ominaisuudet. Esimerkiksi voit käyttää `wget`-komentoa ladataksesi sivun ja tallentaaksesi sen tiedostoon:

```Bash
#!/bin/bash
# Lataa sivu ja tallenna se tiedostoon
wget http://www.example.com -O example.html
```

Kuten huomaat, Bash-skriptit tarjoavat helpon ja tehokkaan tavan ladata verkkosivuja.

## Syvemmälle aiheeseen

On olemassa monia tapoja ladata verkkosivuja Bash-skriptien avulla, ja edellä mainittujen `curl` ja `wget` -komentojen lisäksi on olemassa myös muita vaihtoehtoja, kuten `lynx` ja `links`. Voit myös käyttää `awk`, `grep` ja muita Bash-työkaluja käsitelläksesi ja jalostaa ladattua sivua.

Lisäksi voit käyttää `cron`-työkalua ajastamaan sivujen lataamista haluamallasi tavalla. Esimerkiksi voit luoda Bash-skriptin, joka lataa päivittäin sivun ja tallentaa sen tiedostoon käyttäen `cron`ia.

## Katso myös

- [Bash-opas (suomeksi)](https://www.tutorialspoint.com/unix_commands/bash.htm)
- [`curl` dokumentaatio](https://curl.haxx.se/docs/)
- [`wget` dokumentaatio](https://www.gnu.org/software/wget/manual/wget.html)