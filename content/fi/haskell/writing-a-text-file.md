---
title:    "Haskell: Tekstitiedoston kirjoittaminen"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Haluan aloittaa tämän blogipostauksen kysymällä: Miksi kirjoittaisit tekstitiedoston Haskell-ohjelmointikieltä käyttäen? Vaikka tekstitiedostot voivat vaikuttaa vanhanaikaisilta, niillä on edelleen monia käyttötarkoituksia, kuten tallentaa ja jakaa dataa tai luoda yksinkertaisia tiedostoja. Haskellin avulla voit tehdä tämän nopeasti ja helposti, ja tässä blogipostauksessa opit miten.

## Kuinka tehdä

Aloitetaan luomalla yksinkertainen tekstitiedosto käyttämällä Haskellia. Voit aloittaa luomalla uuden tiedoston nimeltä ```tekstitiedosto.hs```. Sitten voit lisätä seuraavan koodin tiedostoon:

```Haskell
main = do
  writeFile "tekstitiedosto.txt" "Tämä on esimerkki tekstitiedostosta, luotu Haskellilla."
```

Tämä koodi käyttää Haskellin ```writeFile``` -funktiota luodakseen uuden tekstitiedoston nimeltä ```tekstitiedosto.txt``` ja kirjoittaa siihen annetun tekstin. Voit ajaa tämän tiedoston komentoriviltä tai Haskellin REPL-näkymästä.

Voit myös lukea olemassa olevan tekstitiedoston käyttämällä ```readFile``` -funktiota, esimerkiksi:

```Haskell
main = do
  sisalto <- readFile "tekstitiedosto.txt"
  print sisalto
```

Tämä koodi tulostaa kyseisen tekstitiedoston sisällön. Voit myös muokata tiedostoa luomalla uuden ```String``` -muuttujan ja käyttämällä ```writeFile``` -funktiota uuden sisällön kirjoittamiseen.

## Syvä Sukellus

Haskellin tiedostojen käsittely perustuu IO (Input/Output) -monadiin, joka mahdollistaa sen, että funktiot voivat toimia IO-toimintona. Näin funktiot, kuten ```writeFile``` ja ```readFile```, voivat muuttaa tiedostoa joko kirjoittamalla siihen tai lukemalla siitä.

On myös tärkeää muistaa, että tiedoston käsittelyntulisi olla osa laajempaa käsiteltävänä olevaa IO-toimintaa, jotta tiedoston muutoksia voidaan hallita ja varmistaa, että tiedosto sulkeutuu oikein. Samoin, kun luot uuden tiedoston, voit sisällyttää sen omaan IO-toimintoonsa ja suorittaa sen järjestelmän IO-monadissa.

## Katso myös

- [Haskellin virallinen dokumentaatio tiedoston käsittelystä](https://www.haskell.org/tutorial/io.html)
- [Lyhyt opas Haskellin IO-monadiin](https://wiki.haskell.org/All_About_Monads#The_IO_monad)