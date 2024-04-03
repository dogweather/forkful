---
date: 2024-01-27 20:34:50.002826-07:00
description: "Miten: PHP tarjoaa useita toimintoja satunnaisten numeroiden generoimiseen,\
  \ mutta yleisimmin k\xE4ytetyt ovat `rand()`, `mt_rand()` ja kryptografisiin\u2026"
lastmod: '2024-03-13T22:44:56.651556-06:00'
model: gpt-4-0125-preview
summary: "PHP tarjoaa useita toimintoja satunnaisten numeroiden generoimiseen, mutta\
  \ yleisimmin k\xE4ytetyt ovat `rand()`, `mt_rand()` ja kryptografisiin tarkoituksiin\
  \ `random_int()`."
title: Satunnaislukujen generointi
weight: 12
---

## Miten:
PHP tarjoaa useita toimintoja satunnaisten numeroiden generoimiseen, mutta yleisimmin käytetyt ovat `rand()`, `mt_rand()` ja kryptografisiin tarkoituksiin `random_int()`.

Yksinkertaisen satunnaisluvun generoimiseksi välillä 0 ja getrandmax() (suurin mahdollinen arvo, jonka `rand()` voi palauttaa), voit käyttää:

```PHP
echo rand();
```

Tarkemmalle välille, kuten 1:n ja 100:n välille:

```PHP
echo rand(1, 100);
```

`mt_rand()` on kuitenkin parempi valinta nopeuden ja satunnaisuuden kannalta:

```PHP
echo mt_rand(1, 100);
```

Tuloksena molemmissa voisi olla mikä tahansa luku 1:n ja 100:n välillä satunnaistamisen mukaan, esim., `42`.

Kryptografisissa tai turvallisuuskonteksteissa, missä ennalta-arvaamattomuus on olennaista, `random_int()` on suositeltava valinta, sillä se luo kryptografisesti turvallisia pseudo-satunnaislukuja:

```PHP
echo random_int(1, 100);
```

Jälleen, tuloksena on satunnainen luku 1:n ja 100:n välillä, kuten `84`, mutta vahvemmalla satunnaisuuden takeella.

## Syväsukellus
`rand()`-funktio on ollut osa PHP:ta sen alkuaikoina, toimien alkuperäisenä menetelmänä satunnaisten numeroiden generoimiseen. Se ei kuitenkaan ole paras vaihtoehto sovelluksille, jotka vaativat korkeaa satunnaisuuden astetta sen suhteellisen ennustettavan algoritmin vuoksi.

`mt_rand()`, joka esiteltiin PHP 4:ssä, perustuu Mersenne Twister -algoritmiin - se on huomattavasti parempi nopeuden ja sen kyvyn tuottaa satunnaisuutta verrattuna `rand()`-funktioon. Se nousi nopeasti suosituksi vaihtoehdoksi useimmille ei-kryptografisille tarpeille.

Turvallisuusherkissä sovelluksissa, `random_int()` esiteltiin PHP 7:ssä tuottamaan kryptografisesti turvallisia pseudo-satunnaislukuja käyttäen järjestelmän satunnaislukugeneraattorin satunnaisia tavuja. Se on huomattavasti turvallisempi kuin `rand()` tai `mt_rand()`, tehden siitä parhaan valinnan tokenien, avainten tai muiden elementtien generoimiseen, joissa ennustettavuus voisi johtaa turvallisuusriskeihin.

Huolimatta näistä parannuksista, on ratkaisevan tärkeää valita oikea funktio sovelluksen kontekstin perusteella. Yleiskäyttöön `mt_rand()` riittää, mutta mihin tahansa, joka voitaisiin kohdentaa tai hyväksikäyttää, `random_int()` on oikea valinta, tarjoten sekä satunnaisuutta että turvallisuutta.
