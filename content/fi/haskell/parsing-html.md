---
title:                "Haskell: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## Miksi HTML-analysointi on tärkeää?

Jos olet koskaan käyttänyt verkkosivuja tai web-sovelluksia, olet todennäköisesti ollut tekemisissä HTML-koodin kanssa. HTML on merkintäkieli, jota käytetään sivustojen rakentamiseen ja ulkoasun luomiseen. Jotta voitaisiin käsitellä ja manipuloida sivustojen sisältöä, on usein tarpeen analysoida HTML-koodia. Tässä blogikirjoituksessa tutustumme siihen, miten voit analysoida HTML-koodia käyttäen Haskell-ohjelmointikieltä.

## Miten tehdä se?

HTML-koodin analysointi Haskellilla on mahdollista käyttämällä kirjastoa nimeltä "tagsoup". Tämä kirjasto tarjoaa valmiita funktioita, joilla voit lukea ja käsitellä HTML-elementtejä. Esimerkiksi hyödyllinen funktio `parseTags` lukee HTML-koodin ja palauttaa listan "Tag" -tyyppisistä elementeistä. Voit käyttää tätä funktiota esimerkiksi seuraavalla tavalla:

```Haskell
import Text.HTML.TagSoup (parseTags)

html = "<html><head><title>Hello World</title></head><body><h1>Welcome</h1></body></html>"

tags = parseTags html

print tags -- [TagOpen "html" [],TagOpen "head" [],TagOpen "title" [],TagText "Hello World",TagClose "title",TagClose "head",TagOpen "body" [],TagOpen "h1" [],TagText "Welcome",TagClose "h1",TagClose "body",TagClose "html"]
```

Yllä olevassa koodissa luodaan ensin muuttuja `html`, joka sisältää HTML-koodia. Sitten `parseTags` -funktiota käytetään lukemaan HTML ja palauttamaan lista `tags`. Lopuksi tulostetaan tämä lista, ja näemme kaikki elementit, jotka HTML-koodista löytyvät.

Voit myös käyttää Tagsoup-kirjastoa laskemaan tiettyjen elementtien määrää HTML-koodissa. Esimerkiksi jos haluat tietää, kuinka monta otsikkoa `<h1>` on HTML-koodissa, voit käyttää funktiota `count`.

```Haskell
import Text.HTML.TagSoup (parseTags, Tag(..), headTag, (~/=))

html = "<html><head><title>Hello World</title></head><body><h1>Welcome</h1><p>Paragraph</p><h1>Another heading</h1></body></html>"

tags = parseTags html

count = length $ filter isHeading tags
  where isHeading (TagOpen "h1" _) = True
        isHeading _ = False

print count -- 2
```

Yllä olevassa koodissa käytetään funktiota `filter`, joka suodattaa listan elementtejä halutun ehdokkaan mukaan. Tässä tapauksessa suodatetaan `tags`-lista niin, että vain `<h1>`-elementit jäävät jäljelle. Sitten käytetään `length`-funktiota laskemaan suodatetun listan pituus, jolloin saadaan haluttu laskettu arvo.

## Syvempi sukellus

Tagsoup-kirjastolla on monia muita hyödyllisiä funktioita, jotka voit löytää sen dokumentaatiosta. Voit myös käyttää kirjastoa tekemään monimutkaisempia operaatioita, kuten hakemaan tiettyjä elementtejä tai muokkaamaan HTML-koodia.

On kuitenkin hyvä huomata, että Tagsoup-kirjasto ei ole täydellinen. Se ei pysty käsittelemään kaikkia mahdollisia HTML-merkintöjä ja se sa