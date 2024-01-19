---
title:                "HTML:n jäsentäminen"
html_title:           "Bash: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

HTML:n jäsentäminen tarkoittaa HTML-koodin rakenteen lukemista ja analysointia. Ohjelmoijat tekevät tämän ymmärtääkseen ja käsitelläkseen verkkosivujen sisältöä ohjelmoimallaan tavalla.

## Kuinka näin:

Tässä on yksinkertainen esimerkki HTML:n jäsentämisestä Haskellin "tagsoup" -kirjaston avulla

```Haskell
import Text.HTML.TagSoup

haeLinkit :: String -> [String]
haeLinkit = 
  map (fromAttrib "href") 
  . filter (~== "<a href>")
  . parseTags
```

Tämä funktio hakee kaikki linkit annetusta HTML-koodista.

Esimerkki sen suorittamisesta:

```Haskell
> haeLinkit "<html><a href=\"http://example.com\"></a></html>"
["http://example.com"]
```

## Syvempi sukellus

HTML-jäsentämisen käyttö on kasvanut verkkosivujen dynaamisuuden ja monimutkaisuuden kasvaessa. Historiallisesti ohjelmoijat ovat luoneet omia jäsentimiä, mutta nyt on saatavilla erilaisia kirjastoja, kuten Haskellin tagsoup.

Kuten monien ohjelmointitehtävien kohdalla, myös HTML-jäsentämisessä on erilaisia tapoja. Yksi alternative jäsentämiselle on 'regex' tai säännölliset lausekkeet, mutta ne voivat olla hankalia ja virheherkkiä monimutkaisen HTML:n käsittelyssä.

Jäsentämisen tekninen toteutus riippuu paljon käytetystä kirjastosta. Tagsoup-kirjasto esimerkiksi luo listan tunnisteista, joita voidaan sitten manipuloida.

## Katso myös

[Tagsoup-kirjaston dokumentaatio](http://hackage.haskell.org/package/tagsoup)

[W3Schoolsin HTML-tutorial](https://www.w3schools.com/html/)

[Haskell Café -keskustelualue](https://mail.haskell.org/pipermail/haskell-cafe/)