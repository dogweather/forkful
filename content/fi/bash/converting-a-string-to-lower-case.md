---
title:                "Merkkijonon muuntaminen pieniksi kirjaimiksi"
html_title:           "Arduino: Merkkijonon muuntaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuntaminen pieniksi kirjaimiksi"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Muuttaminen merkkijono pieniksi kirjaimiksi tarkoittaa sisällön käsittelyä siten, että kaikki merkkijonon suuret kirjaimet muuttuvat pieniksi kirjaimiksi. Ohjelmoijat tekevät tämän muodostaakseen yhtenäisyyttä ja välttääkseen tietojen väärinkäsitykset, koska Bash on kirjainkoosta riippuvainen.

## Näin tehdään:

Bash-ohjelmassa voit tuoda esille pienet kirjaimet käyttämällä tr-komentoa seuraavasti:

```Bash
echo 'Hei, Suomi!' | tr '[:upper:]' '[:lower:]'
```

Edellä oleva käskee Bashin tulostamaan 'Hei, Suomi!' jossa kaikki suuret kirjaimet muutetaan pieniksi kirjaimiksi. Tämä tuottaa seuraavan tuloksen:

```Bash
hei, suomi!
```

## Syvä Sukellus:

Historiallisesta näkökulmasta, tr-komento on peräisin Unix-järjestelmän varhaisista päivistä ja on ollut osa POSIX-standardia vuodesta 1993 alkaen.

Tärkeä vaihtoehto sisällytetty Bash-ohjelmaan on `${variable,,}` syntaksi, jossa 'variable' on muuttuja, jonka arvo haluat muuttaa:

```Bash
variable='Hei, Suomi!'
echo "${variable,,}"
```

Tämä tulostaa saman tuloksen kuin tr-komento:

```Bash
hei, suomi!
```

Muistutus: bash-ohjelman `${variable,,}` syntaksi tuli käyttöön version 4.0 julkaisussa, joten se ei välttämättä ole saatavilla vanhemmissa järjestelmissä.

## Katso myös:

1. Bash-kielen käsikirja: https://www.gnu.org/software/bash/manual/bash.html
2. Yksityiskohtainen opas TR-komentoon: https://www.geekhideout.com/dir/2011/09/19/unix-tr-command-examples/
3. Alkuperäinen POSIX-standardi: https://pubs.opengroup.org/onlinepubs/9699919799/