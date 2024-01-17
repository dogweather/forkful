---
title:                "Virheenkorjaustulosteen tulostus"
html_title:           "Fish Shell: Virheenkorjaustulosteen tulostus"
simple_title:         "Virheenkorjaustulosteen tulostus"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Debug-tulostuksen tulostaminen on ohjelmoijille tärkeä työkalu virheiden korjaamiseen ja ohjelman toiminnan havainnointiin. Se auttaa löytämään ongelmallisia kohtia koodista ja varmistamaan, että ohjelma toimii halutulla tavalla.

## Miten:
Koodin esimerkit ja tulostus näytetään ```Fish Shell ... ``` -koodilohkossa.

Esimerkiksi, jos haluat tulostaa luvun 10, käytä seuraavaa koodia:

```
echo 10
```

Ja ohjelman tulostus näyttää seuraavalta:

```
10
```

## Syväsukellus:
Debug-tulostuksen käyttö on vakiintunut käytäntö ohjelmoinnissa jo vuosikymmenten ajan ja sitä käytetään lähes kaikissa ohjelmointikielissä. Vaikka on olemassa vaihtoehtoisia työkaluja, debug-tulostus on usein nopein ja kätevin tapa havaita ja korjata virheitä ohjelmassa.

Fish Shellin avulla voit myös käyttää erilaisia ​​komentoja, kuten ```print``` tai ```printf```, joiden avulla voit muotoilla ja tulostaa tulosteen haluamallasi tavalla.

## Katso myös:
- [Fish Shellin viralliset verkkosivut](https://fishshell.com/)
- [Fish Shellin dokumentaatio](https://fishshell.com/docs/current/index.html)
- [Ohjeita debug-tulostuksen käyttöön](https://medium.com/@jorge_rouae/como-usar-el-output-debugging-en-tu-c%C3%B3digo-5ece0883d6f8)