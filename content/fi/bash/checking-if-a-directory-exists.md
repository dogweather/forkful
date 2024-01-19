---
title:                "Tarkistetaan, onko hakemisto olemassa"
html_title:           "Bash: Tarkistetaan, onko hakemisto olemassa"
simple_title:         "Tarkistetaan, onko hakemisto olemassa"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Bashissa, toiminnallisuus, jolla tiedetään, onko hakemisto olemassa, on koodin pätkä, joka tarkistaa, onko tietty hakemisto olemassa ennen kuin ohjelma jatkaa toimintaansa. Tätä käytetään välttämään virheitä, jotka syntyvät, kun yrittää käyttää olematonta hakemistoa.

## Näin se tehdään:
Bashin sisäänrakennettu `[-d DIR]` -testi tarkistaa, onko hakemisto olemassa. Se palauttaa "true" (arvo 0), jos hakemisto on olemassa.

```Bash
#!/bin/bash
hakemisto="/polku/hakemistoon"
if [ -d "$hakemisto" ]; then
    echo "Hakemisto on olemassa."
else
    echo "Hakemisto ei ole olemassa."
fi
```
Esimerkin tulostus voisi näyttää tältä:

```Bash
Hakemisto on olemassa.
```
tai
```Bash
Hakemisto ei ole olemassa.
```
## Syvällisemmin
Bashin sisäänrakennettu `-d` -testi on peräisin Unixin varhaisista päivistä, ja sitä on käytetty samalla tavalla alun perin syntyneissä Bourne shellissä. On myös muita tapoja tarkistaa, onko hakemisto olemassa, kuten `[[ -d $hakemisto ]]` kaksoishakasuluissa, joka on nykyaikaisempi ja välttää joitakin "test"-käskyn ongelmia. On myös tärkeää muistaa, että tämä tarkistus ei kerro, onko kyseisellä käyttäjällä kyseiseen hakemistoon, vaan vain, onko hakemisto olemassa.

## Katso myös
- GNU Bash Manual: [`test`](https://www.gnu.org/software/bash/manual/bash.html#Bourne-Shell-Builtins)
- `man test`: Komentorivin manuaalinen sivu sisäänrakennetulle `test`-käskylle eri unix-järjestelmissä.
- Nachum Dotanin kirjoitus [Test command in Unix/Linux](https://www.geeksforgeeks.org/test-command-linux-examples/)