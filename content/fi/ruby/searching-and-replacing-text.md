---
title:                "Ruby: Tekstin etsiminen ja korvaaminen"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

Joskus ohjemointikoodissa on tarpeen korvata tekstiä toisella, esimerkiksi korjataksesi kirjoitusvirheitä tai muuttaaksesi tiettyjä termejä koodissasi. Tämä voi säästää paljon aikaa ja vaivaa, kun käsittelet suuria kooditiedostoja.

## Miten

Ruby:ssa on useita tapoja etsiä ja korvata tekstiä. Yksi tapa on käyttää `gsub`-metodia, joka korvaa kaikki esiintymät haluamallasi tekstillä. Esimerkiksi, jos haluat korvata kaikki "hello" tekstit "hei", voit käyttää seuraavaa koodia:

```Ruby
string = "Hello world"
string.gsub!("hello", "hei")
puts string
```

Tämän koodin tulostus olisi "Hei world". Voit myös etsiä ja korvata tekstiä rajatulla määrällä esiintymiä käyttämällä `sub`-metodia:

```Ruby
string = "Hello world"
string.sub!("hello", "hei")
puts string
```

Tämän koodin tulostus olisi "Hei world" vain ensimmäisellä esiintymällä.

## Syvällinen sukellus

Kun käytät `gsub`- ja `sub`-metodeja, voit myös antaa sille lohkoa, joka määrittelee tarkemman korvaussäännön. Lohkon tulisi ottaa parametri, joka edustaa korvatun tekstin. Voit myös käyttää erityisiä kaavailemia regex-sääntöjä määrittämään millaiset tekstit korvataan.

Esimerkiksi, jos haluat vaihtaa kaikki numerot *x*:ään, ja lisätä sitten numeron eteen ja jälkeen asteriskit, voit käyttää seuraavaa koodia:

```Ruby
string = "1 2 3 4 5"
string.gsub!(/\d/, 'x')
string.gsub!(/(x)/, '*\1*')
puts string
```

Tämän koodin tulostus olisi "*x* *x* *x* *x* *x*".

## Katso myös

- [Ruby dokumentaatio: String Class](https://ruby-doc.org/core-3.0.0/String.html)
- [Ruby Regex Tutorial](https://www.rubyguides.com/2015/06/ruby-regex/)