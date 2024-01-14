---
title:                "Ruby: Alimerkkijonojen erottelu"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

# Miksi

Substringien erottaminen on tärkeä osa Ruby-ohjelmointikieltä. Se antaa sinulle kyvyn etsiä ja käsitellä tiettyjä osia merkkijonosta, mikä helpottaa monien ongelmien ratkaisemista. Se on erityisen tärkeää, kun työskentelyssä yksittäisen sanan tai lauseen kanssa on oleellista, ja haluat välttää manuaalisen tekstin pilkkomisen vaivan.

# Miten

Substringien erottaminen Rubyssä on helppoa. Voit käyttää nimettyä alaindeksimerkintää ja sisällyttää indeksin aloituskohdan ja lopetuskohdan välille.

```
Ruby
string = "Tervetuloa!"
string[0,4] # tulostaa "Terv"
```

Voit myös käyttää indeksivälilehteä, joka sallii indeksien käyttämisen taulukoissa. Tässä tapauksessa voit antaa aloitus- ja lopetusindeksin välisen alueen, josta saat alastringin.

```
Ruby
string = "Tervetuloa!"
string[3..7] # tulostaa "etul"
```

# Syvällinen tarkastelu

Rubyssä substringien erottaminen on mahdollista myös käyttäen `slice`-metodia. Tämän metodin avulla voit antaa myös negatiivisia indeksejä, jotka aloittavat merkkijonon lopusta.

```
Ruby
string = "Tervetuloa!"
string.slice(-2) # tulostaa "a"
```

Voit myös käyttää `slice!`-metodia, joka muuttaa alkuperäistä merkkijonoa ja poistaa erotetun alastringin.

Seuraa allaolevia linkkejä lisätietoihin substringien erottamisesta Rubyssa:

- Rubyn virallinen dokumentaatio: https://ruby-doc.org/core-2.7.1/String.html
- RubyGuides: https://www.rubyguides.com/2019/05/ruby-substring/
- SitePoint: https://www.sitepoint.com/ruby-string-methods/

# Katso myös

- Rubyn oppimateriaalit: https://docs.ruby-lang.org/en/3.0.0/Quickstart.html
- Ruby Codecademy: https://www.codecademy.com/learn/learn-ruby