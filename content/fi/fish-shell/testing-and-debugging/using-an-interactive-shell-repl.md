---
date: 2024-01-26 04:14:19.111952-07:00
description: "Kuinka: Fishiss\xE4 interaktiivinen kuori on oletustila, kun k\xE4ynnist\xE4\
  t sen. T\xE4lt\xE4 se n\xE4ytt\xE4\xE4 toiminnassa."
lastmod: '2024-03-13T22:44:56.996189-06:00'
model: gpt-4-0125-preview
summary: "Fishiss\xE4 interaktiivinen kuori on oletustila, kun k\xE4ynnist\xE4t sen."
title: "Interaktiivisen komentotulkin (REPL) k\xE4ytt\xF6"
weight: 34
---

## Kuinka:
Fishissä interaktiivinen kuori on oletustila, kun käynnistät sen. Tältä se näyttää toiminnassa:

```Fish Shell
> set color blue
> echo "Taivas on $color"
Taivas on sininen
```

Voit myös ajaa sisäänrakennettuja funktioita ja leikkiä komentosubstituutioiden kanssa:

```Fish Shell
> function cheer
      echo "Go Fish $argv!"
  end
> cheer Koodaajat
Go Fish Koodaajat!
```

Ei vain funktioiden määrittelyä, voit suorittaa koodinpätkiä lennosta ja nähdä tuloksen välittömästi:

```Fish Shell
> math "40 / 2"
20
```

## Syväsukellus
REPLien konsepti juontaa juurensa aina 1960-luvun Lisp-ohjelmointikieleen. Tämä vuorovaikutteisen ohjelmoinnin muoto loi mittapuun ympäristöille kuten Pythonin `ipython` ja Rubyn `irb`. Fish jatkaa trendiä keskittyen käyttäjäystävällisyyteen ja vuorovaikutteiseen käyttöön.

Fish eroaa muista kuorista, kuten Bashista, siinä, että se on suunniteltu interaktiivisuutta silmällä pitäen alusta alkaen. Se tarjoaa syntaksikorostuksen, automaattiset ehdotukset ja välilehdellä täydennykset, jotka tekevät sen käytöstä tehokasta REPL-tyylisessä työnkulussa. Parempi vielä, komentosi muistetaan ja ovat haettavissa, mikä tekee toistuvasta testauksesta tuulen nopeaa.

Fishin REPLin vaihtoehtoja voisivat olla `bash` tai `zsh`, kun ne yhdistetään laajennuksiin kuten `bash-completion` tai `oh-my-zsh`, mutta Fish tarjoaa yleensä rikkaamman kokemuksen suoraan laatikosta.

## Katso Myös:
- Fish Documentation: https://fishshell.com/docs/current/index.html
- Mielenkiintoinen vertailu Fishin ja muiden kuorien välillä: https://www.slant.co/versus/2209/3686/~fish_vs_bash
- Syväsukellus REPL:eihin: https://en.wikipedia.org/wiki/Read–eval–print_loop
- Vuorovaikutteinen ohjelmointi Lispissä, historiallinen katsaus: http://www.paulgraham.com/ilisp.html
