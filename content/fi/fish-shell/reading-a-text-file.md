---
title:                "Tekstitiedoston lukeminen"
html_title:           "Lua: Tekstitiedoston lukeminen"
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Tekstitiedoston lukeminen tarkoittaa tiedostossa olevan tekstin tulkkausta sen sisällön ymmärtämiseksi. Ohjelmoijat tekevät tämän tietojen noutamiseksi tiedostoista tai tiedostojen sisällön manipuloimiseksi.

## Miten tehdään: 

Lukeminen on yksinkertaista Fish Shell-ohjelmassa. Käytämme `cat`-komentoa näyttääksemme tiedoston sisällön:

```Fish Shell 
cat /polku/tiedostoon.txt
```

Edellä oleva koodi näyttää tekstin, joka sijaitsee polussa `/polku/tiedostoon.txt`. Toisen esimerkin voisi olla lukeminen riveittäin käyttäen `read`-komentoa:

```Fish Shell 
for rivi in (cat /polku/tiedostoon.txt)
    echo $rivi
end
```

Nämä komentorivit lukevat tiedoston `/polku/tiedostoon.txt` rivi kerrallaan ja tulostavat sen.

## Syvemmälle:

`cat`-komento, jonka käytämme Fish Shell-ohjelmassa, on peräisin Unix-järjestelmästä ja sitä on käytetty jo 1970-luvulta lähtien tekstitiedostojen lukemiseen. Vaihtoehtoisesti voidaan käyttää `more` tai `less` komentoja, jotka antavat enemmän kontrollia tekstin selaamiseen.

On tärkeää ymmärtää, että nämä komennot lukevat tiedostoja tavu kerrallaan, joten suurten tiedostojen kanssa saattaa tulla suorituskykyongelmia.

## Katso myös: 

Fish Shell -ohjelmoinnin oppimista varten kannattaa tutustua seuraaviin sivustoihin:

- Fish Shellin virallinen dokumentaatio: https://fishshell.com/docs/current/index.html
- Sovelluksia ja esimerkkejä Fish Shell scripteistä: https://github.com/jorgebucaran/awesome.fish
- Fish Shell yhteisön keskustelupalsta: https://gitter.im/fish-shell/fish-shell