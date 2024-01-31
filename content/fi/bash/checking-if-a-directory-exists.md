---
title:                "Onko hakemisto olemassa? Tarkistaminen"
date:                  2024-01-19
html_title:           "Bash: Onko hakemisto olemassa? Tarkistaminen"
simple_title:         "Onko hakemisto olemassa? Tarkistaminen"

category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Tarkistamme, onko hakemisto olemassa, koska se vaikuttaa seuraaviin toimiin – et halua yrittää lukea kirjoittamatonta hakemistoa. Se on virheiden hallintaa: eteenpäin vain, jos polku on validi.

## How to: (Kuinka tehdä:)
```Bash
# Tarkistetaan, onko hakemisto olemassa
if [ -d "$DIRECTORY" ]; then
  echo "Hakemisto $DIRECTORY löytyy."
else
  echo "Hakemisto $DIRECTORY ei ole olemassa."
fi
```

Esimerkkitulostus, kun hakemisto on olemassa:
```
Hakemisto /home/kayttaja/dokumentit löytyy.
```

Esimerkkitulostus, kun hakemistoa ei ole:
```
Hakemisto /home/kayttaja/vaarahakemisto ei ole olemassa.
```

## Deep Dive (Sukellus syvemmälle)
Ennen kuin Bash oli suosittu, ihmiset käyttivät muita komentotulkkeja, kuten sh tai csh. Bash, lyhenne "Bourne Again Shell", tuli suosituksi sen joustavuuden ja paranneltujen ominaisuukset ansiosta.

Vaihtoehtona `if`-testille voisi käyttää `test`-komentoa tai sen synonyymiä `[ ]`. Saatavilla on myös modernimpia työkaluja, kuten `[[ ]]` rakenteet ja `test -e` mutta `-d` on oikea valinta kun selvästi tarkistetaan hakemistoja.

Hakemiston olemassaolon tarkistaminen Bash-skriptissä on tärkeää mm. silloin, kun:
- Luodaan tiedostoja tai hakemistoja vain, jos kohdehakemisto on olemassa.
- Tehdään puhdistustoimia, kuten vanhojen lokejen poisto.

## See Also (Katso myös)
- [Bash manuaali](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html)
- [Advanced Bash-Scripting Guide](https://www.tldp.org/LDP/abs/html/)
- [ShellCheck](https://www.shellcheck.net/), staattinen analyysityökalu skriptien virheiden löytämiseen.
