---
title:                "Merkkijonosta päivämäärän jäsentäminen"
date:                  2024-01-20T15:36:28.342089-07:00
html_title:           "Bash: Merkkijonosta päivämäärän jäsentäminen"
simple_title:         "Merkkijonosta päivämäärän jäsentäminen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Mikä & Miksi?)
Tietojen jäsennys merkkijonosta tarkoittaa päivämäärän erottelua tekstiformaatista. Ohjelmoijat tekevät sen, koska päivämäärät on saatava muunnettua eri järjestelmien ymmärtämään muotoon. 

## How to: (Kuinka tehdään:)
Fish Shell tarjoaa työkaluja päivämäärän jäsentämiseen. Voit muuntaa merkkijonon päivämääräksi hyödyntäen `string` ja `date` komentoja:

```Fish Shell
set date_str "2023-04-01"
set epoch_time (date -ud "$date_str" +"%s")
echo $epoch_time
```

Esimerkin tulostus näyttäisi tältä, joka on Unix-aikaleima:

```
1679942400
```

Toinen esimerkki, missä muutetaan aikaleima normaaliksi päivämääräksi:

```Fish Shell
set epoch_time 1679942400
set normal_date (date -ur "$epoch_time" +"%Y-%m-%d")
echo $normal_date
```

Tulostuu:

```
2023-04-01
```

## Deep Dive (Syväsukellus)
Ennen Unix-aikaleimoja ja standardoituja päivämäärämuotoja, päivämääräkäsittely oli hankalaa. Yksi ratkaisu oli luoda omia funktioita merkkijonojen purkamiseen. 

Fish Shell, kuten monet muut kuoret, ei itsessään tarjoa päivämääräjäsennystä, vaan se nojaa ulkoisiin komentoihin kuten `date`. Tämä on käyttöjärjestelmistä riippuvaista; esimerkiksi GNU date ja BSD date saattavat käyttäytyä eri tavalla. Varmista käyttämäsi version sopivuus.

Päivämääräkomennolle annettavat muotoiluoptiot (`+%Y-%m-%d`) määrittelevät, miten tulostettu aika esitetään. Komento `date -u` käyttää UTC-aikaa ja `-r` tulkitsee annetun Unix-aikaleiman. Fish:n vahvuuksia on että se integroituu saumattomasti käyttöjärjestelmän työkaluihin, kuten `date` komentoon, mahdollistaen tehokkaan päivämäärän käsittelyn yhdessä muiden Unix-komentojen kanssa.

## See Also (Katso Myös)
- Fish Shell dokumentaatio: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- GNU Coreutils `date` komento: [https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- Unix-aikaleimat sekä niiden muuntaminen: [https://en.wikipedia.org/wiki/Unix_time](https://en.wikipedia.org/wiki/Unix_time)