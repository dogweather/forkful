---
title:                "Fish Shell: Merkkijonon muuttaminen pienaakkosiksi"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit muuntaa merkkijonon pieniksi kirjaimiksi Fish Shell -ohjelmointikielen avulla? Yksi syy voi olla yhtenäisyyden luominen, jos sinulla on tarve vertailla merkkijonoja ilman, että kirjainkoko vaikuttaa vertailun tulokseen. Lisäksi pieniksi kirjaimiksi muuttaminen voi auttaa estämään mahdollisia kirjoitusvirheitä.

## Kuinka

Fish Shell -koodiesimerkit ja tulostulosteet seuraavat alempana olevassa koodilohkossa.

```Fish Shell
# Alustetaan muuttuja merkkijonolla

set string "HeLLo WorLD"

# Käytetään 'string tolower' funktiota muuttamaan merkkijonon kirjainkoko pieneksi

echo $string | string tolower

```

Tulostuloste:

`hello world`

## Syvempi sukellus

Merkkijonon muuntaminen pieniksi kirjaimiksi Fish Shell -ohjelmointikielen avulla vaatii toimenpiteitä, jotka ovat suurimmaksi osaksi samanlaisia kuin useimmissa muissakin ohjelmointikielissä. Mutta miksi käyttäisit Fish Shell -ohjelmointikieltä sen sijaan, että vain käyttäisit käyttöjärjestelmäsi tarjoamaa sisäänrakennettua toimintoa? Fish Shell mahdollistaa joustavamman ja monipuolisemman lähestymistavan merkkijonojen käsittelyyn ja muuntamiseen. Se myös helpottaa merkkijonojen vertailua ja käsittelemistä muihin ohjelmointikieliin verrattuna.

## Katso myös

http://fishshell.com/docs/current/cmds/string.html#tolower-command

https://fishshell.com/docs/current/functions.html#lowercase

https://fishshell.com/docs/current/cmds/set.html#built-in-commands