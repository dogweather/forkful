---
title:                "Tarkistetaan, onko hakemisto olemassa"
html_title:           "C: Tarkistetaan, onko hakemisto olemassa"
simple_title:         "Tarkistetaan, onko hakemisto olemassa"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? - Mikä ja Miksi?
Hakemiston olemassaolon tarkistaminen tarkoittaa, että selvitetään, onko tietyllä polulla oleva hakemisto olemassa. Ohjelmoijat tekevät tämän, koska se auttaa välttämään virheitä, kun he yrittävät käyttää hakemistoa, jota ei ole olemassa.

## How to: - Kuinka:
```Fish Shell
if test -d /path/to/directory
    echo "Directory exists"
else
    echo "Directory does not exist"
end
```

Esimerkkituloste:
```
Directory exists
```
tai jos hakemistoa ei ole:
```
Directory does not exist
```

## Deep Dive - Syväsukellus:
Vanhat Unix-perinteet elävät `test`-komennossa; Fish Shell käyttää sitä, kuten muutkin shellit. Vaihtoehtoisia tapoja tarkistaa hakemisto on käyttää funktioita tai skriptejä, mutta `test` ylläpitää yksinkertaisuutta ja siirrettävyyttä. Kun test-komento suoritetaan `-d`-vivun kanssa, se tarkistaa, viittaako polku oikeasti olemassa olevaan hakemistoon. Teknisesti Fish Shell välittää tämän tarkistuksen Unix-kernelille joka hoitaa työn käyttöjärjestelmän tasolla.

## See Also - Katso Myös:
- Fish Shell:n virallinen dokumentaatio: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)