---
title:                "Kirjoittaminen standardivirheeseen"
html_title:           "Bash: Kirjoittaminen standardivirheeseen"
simple_title:         "Kirjoittaminen standardivirheeseen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Kirjoittaminen standardi virhevirtaan (STDERR) on tapa ilmoittaa käyttäjälle skriptin suorituksen aikaisista ongelmista tai virheistä. Tämä on hyödyllistä esimerkiksi silloin, kun haluat tulostaa vianmääritystietoja tai varoituksia erillään tavallisesta tulosteesta, joka menee standardi tulostevirtaan (STDOUT). Näin käyttäjä näkee selkeästi mitkä tulosteet ovat normaaleja ja mitkä ovat mahdollisia ongelmia.

## Miten tehdä?

Käytä komentoa `echo` tulostamaan haluamasi viesti STDERR:iin seuraavasti:
```
echo "Virhe on tapahtunut" >&2
```
Tämä ohjaa tulosteen STDOUT:sta STDERR:iin lisäämällä merkintä `>&2` komennon perään. Jos haluat tulostaa vain varoituksen, voit ohjata sen STDERR:iin komennolla `echo "Varoitus!" >&2`.

## Syväsukellus

Kirjoittaminen STDERR:iin on yksi tapa hallita ja havaita virheitä ohjelmoinnissa. Toinen tapa on käyttää `exit`-komentoa lopettamaan skriptin suoritus kokonaan ja antamaan sille virhekoodi. Tämä on hyödyllistä esimerkiksi automatisoiduissa tehtävissä, joissa haluat pysäyttää suorituksen jos jokin menee pieleen.

STDERR:iin kirjoittamisella on myös historiallinen merkitys UNIX-järjestelmissä, joissa se on ollut käytössä jo vuodesta 1971 lähtien. Toinen vaihtoehto virheiden hallintaan on käyttää logitiedostoja, joihin kirjoitetaan tietoa skriptin suorituksen aikaisista tapahtumista.

## Katso myös

- [Bashin dokumentaatio](https://www.gnu.org/software/bash/manual/bash.html#redirections)
- [UNIX-historia](https://www.tutorialspoint.com/unix/unix-what-is-shell.htm)