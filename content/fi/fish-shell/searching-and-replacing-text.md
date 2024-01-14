---
title:                "Fish Shell: Tekstin hakeminen ja vaihtaminen"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Miksi

Tekstin etsiminen ja korvaaminen on tärkeä osa ohjelmoinnin ja tiedonkäsittelyn prosessia. Se auttaa meitä nopeasti muuttamaan suuria määriä tekstiä halutun muodon tai sisällön mukaiseksi. Fish Shellissa on joitakin tehokkaita työkaluja tähän tarkoitukseen, ja tässä artikkelissa tarkastelemme niitä syvällisemmin.

# Miten

Fish Shell tarjoaa muutamia erilaisia vaihtoehtoja tekstin etsimiseen ja korvaamiseen. Yksi tapa on käyttää `sed` -komennon muunnelmia, kuten `gsed` tai `sed -i`, jotka tunnistetaan automaattisesti Fish Shellin kautta.

```Fish Shell
# Etsi ja korvaa "hello" tekstillä "hei"
gsed -i 's/hello/hei/g' tiedosto.txt

# Etsi ja korvaa useita sanoja
gsed -i 's/word1/word2/g ; s/word3/word4/g' tiedosto.txt
```

Toinen vaihtoehto on käyttää Fish Shellin sisäänrakennettua `string replace` -toimintoa.

```Fish Shell
# Etsi ja korvaa "hello" tekstillä "hei"
string replace hello hei tiedosto.txt

# Etsi ja korvaa useita sanoja
string replace word1 word2 word3 word4 tiedosto.txt
```

## Syvällisempi sukellus

Fish Shellin `string replace` -toiminto tarjoaa monia hyödyllisiä vaihtoehtoja. Voit esimerkiksi käyttää säännöllisiä lausekkeita, jotta voit määrittää tarkasti, mitä tekstin osia etsiä ja korvata.

```Fish Shell
# Etsi ja korvaa kaikki numerot väliltä 0-9
string replace '[0-9]' 'number' tiedosto.txt
```

Voit myös käyttää `|` -merkkiä, jotta voit suorittaa useita korvauksia samalla kertaa.

```Fish Shell
# Etsi ja korvaa "hello" tekstillä "hei" tai "hi"
string replace hello hei | hi tiedosto.txt
```

Näiden työkalujen lisäksi Fish Shell tarjoaa myös `replace` -toiminnon, joka toimii samalla tavoin kuin `string replace`, mutta se korvaa vain ensimmäisen esiintymän.

# Katso myös

- [Fish Shellin dokumentaatio tekstikorvauksista](https://fishshell.com/docs/current/cmds/string.html#replace)
- [Fish Shellin käyttöohjeet säännöllisistä lausekkeista](https://fishshell.com/docs/current/tutorial.html#tutorialregular-expressions)