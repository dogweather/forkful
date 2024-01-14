---
title:    "Fish Shell: Alastringien erottelu"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Miksi

Substringien uuttaminen on hyödyllinen taito Fish Shell ohjelmoinnissa, sillä se mahdollistaa tiettyjen merkkijonojen erottamisen suuremmista merkkijonoista. Tämä voi auttaa tehostamaan koodia ja säästämään aikaa kirjoittaessa.

## Miten tehdä

Fish Shell tarjoaa kätevän tavan uuttaa substringit käyttämällä sisäänrakennettuja funktioita `string sub` ja `string match`. Esimerkiksi, jos haluamme erottaa merkkijonon "-fishsell" merkkijonosta "this-is-a-fishsell-tutorial", voimme käyttää seuraavaa koodia:

```Fish Shell
string sub -b -q -u "this-is-a-fishsell-tutorial" "-fishsell"
```

Tämä tuottaisi tulosteeksi "this-is-a-tutorial". Käyttämällä `-b` vaihtoehtoa, voimme määrittää, että merkkijonon loppuosa ("fishsell") tulee substrinksi. `-q` vaihtoehto piilottaa tulosteesta lainausmerkit, ja `-u` vaihtoehto tekee tuloksesta ainutlaatuisen.

Toinen tapa uuttaa substringi on käyttää funktiota `string match`, joka palauttaa merkkijonon osan, joka täsmää annetun säännön kanssa. Esimerkiksi, jos haluamme erottaa osan merkkijonosta "fishsell-tutorial", jossa on vain pienet kirjaimet, voimme käyttää seuraavaa koodia:

```Fish Shell
string match -lr "[a-z]+$" "fishsell-tutorial"
```

Tämä tuottaisi tulotseksi "tutorial", sillä vain "tutorial" täsmää sääntöön `[a-z]+$`, joka tarkoittaa, että merkkijonossa tulee olla vain pieniä kirjaimia viimeisen merkin jälkeen.

## Syvällisempi sukellus

Fish Shell tarjoaa myös muita hyödyllisiä toimintoja substringien uuttamiseen, kuten `string replace`, joka korvaa merkkijonon osan toisella merkkijonolla, ja `string trim`, joka poistaa merkkijonosta tiettyjä merkkejä tai sanoja. Näitä funktioita voidaan hyödyntää monin eri tavoin substringien käsittelyssä ja muokkaamisessa.

## Katso myös

- String Manipulation in Fish Shell: https://fishshell.com/docs/current/cmds/string.html
- 10 kätevää Fish Shell ohjetta: https://www.ee.ryerson.ca/~elf/hack/fish.1.html