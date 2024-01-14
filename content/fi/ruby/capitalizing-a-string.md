---
title:                "Ruby: Merkkijonon isoiksi kirjoittaminen"
simple_title:         "Merkkijonon isoiksi kirjoittaminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Jokainen ohjelmaaja käyttää erilaisia metodeja ja toimintoja päivittäin, ja joukossa on myös sellaisia, joita hän ei välttämättä ole käyttänyt aikaisemmin. Yksi tällainen on "capitalize" eli merkkijonon isoiksi kirjaimiksi muuntaminen. Tässä blogikirjoituksessa sukellamme syvälle ja tarkastelemme, miksi ja miten tätä metodia käytetään Ruby-ohjelmoinnissa.

## Miten

Käytä tätä yksinkertaista koodinpätkää muuttaaksesi merkkijonon isommiksi kirjaimiksi:

```Ruby
word = "ruby"
print word.capitalize
```

Koodin tulos tulisi olla "Ruby". Voit myös käyttää capitalize-metodia suoraan merkkijonoon, kuten esimerkissä alla:

```Ruby
print "ruby".capitalize
```

Kumpikin koodinpätkä tuottaa saman tuloksen, joten olet vapaa valitsemaan, kumpaa haluat käyttää. On myös hyvä huomata, että capitalize-metodin avulla voit muuntaa myös vain tietyn osan merkkijonosta, kuten esimerkissä alla:

```Ruby
phrase = "hello world"
print phrase.capitalize
```

Koodin tulos olisi "Hello world". Huomaatko eron? Vain ensimmäinen sana muuttuu isommiksi kirjaimiksi.

## Syvemmälle

Miten sitten tämä capitalize-metodi toimii? Se käyttää Unicode Standardin määrittelemää sääntöjä muuttaakseen merkkijonon ensimmäisen kirjaimen isommaksi. On myös tärkeä muistaa, että capitalize-metodi ei muuta muita kirjaimia kuin ensimmäisen. Esimerkiksi sana "Ruby" pysyy samana, koska kaikki kirjaimet ovat jo isolla.

On myös hyvä huomata, että capitalize-metodi toimii vain kirjaimilla, ei esimerkiksi numeroilla tai välimerkeillä. Tämä johtuu siitä, että näillä ei ole isoja ja pieniä versioita kuten kirjaimilla.

## Katso myös

- Ruby:n virallinen capitalize-dokumentaatio: https://ruby-doc.org/core-2.7.2/String.html#method-i-capitalize
- Selkeä opas capitalize-metodin käyttöön: https://www.rubyguides.com/2019/11/ruby-string-capitalize/
- Unicode Standardin määritelmät: https://www.unicode.org/standard/standard.html