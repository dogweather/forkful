---
title:    "Fish Shell: Tekstin etsiminen ja korvaaminen"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

Miksi sinun kannattaa etsiä ja korvata tekstiä Fish Shell -kielellä? Etsiminen ja korvaaminen on välttämätöntä, kun haluat muokata tai päivittää suuria määriä tekstiä helposti ja nopeasti. Fish Shell tarjoaa käteviä työkaluja tähän tarkoitukseen, joten voit säästää aikaa ja vaivaa muokkausprosessissa.

## Miten

Fish Shell -kielellä tekstien etsiminen ja korvaaminen on erittäin yksinkertaista. Voit käyttää komentoa ```sed```, joka löytyy useimpien Linux-jakeluiden mukana. Se toimii seuraavasti:

```
sed 's/vanhateksti/uusiteksti/g' tiedosto.txt
```

Tämä komento etsii ja korvaa kaikki esiintymät "vanhateksti" tekstillä "uusiteksti" tiedostossa "tiedosto.txt". Jos haluat korvata vain tietyt esiintymät, voit lisätä numeron tai kirjaimen komennon loppuun:

```
sed 's/vanhateksti/uusiteksti/2g' tiedosto.txt
```

Tämä korvaa vain toisen esiintymän, mutta jättää muut ennalleen. Voit myös käyttää erilaisia säännöllisiä lausekkeita etsiäksesi ja korvataksesi tiettyjä tekstin osia. Esimerkiksi, voit käyttää ```/d```-toimintoa poistaaksesi rivejä, jotka sisältävät halutun sanan:

```
sed '/poistettavasana/d' tiedosto.txt
```

Voit myös käyttää komentoa ```grep``` etsiäksesi tiettyjä tekstin osia ja komentoa ```awk``` muokataksesi niitä. Tässä on muutamia esimerkkejä:

```
grep 'etsittäväsana' tiedosto.txt
awk '{print $1}' tiedosto.txt
```

## Syvällinen sukellus

Fish Shell tarjoaa monia työkaluja tekstien etsimiseen ja korvaamiseen, joten suosittelemme tutkimaan niitä ja löytämään itsellesi sopivan tavan. Voit myös käyttää Regex-säännöllisiä lausekkeita tarkempiin haku- ja korvausmalleihin. Ota aikaa oppia ja kokeilla eri vaihtoehtoja, jotta voit optimoida prosessisi ja tehdä siitä mahdollisimman tehokkaan.

## Katso myös

- Fish Shell -dokumentaatio: https://fishshell.com/docs/current/index.html
- Sed-opetusohjelma: https://www.cs.helsinki.fi/u/vahakang/tiedotekniikka/sed.html
- Regex-opetusohjelma: https://regexone.com/