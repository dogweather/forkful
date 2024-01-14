---
title:                "Ruby: Debug-tulostuksen tulostaminen"
simple_title:         "Debug-tulostuksen tulostaminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Ruby-ohjelman debug-tulosteiden tulostaminen on tärkeä osa ohjelmointiprosessia. Debug-tulosteiden tulostaminen auttaa sinua selvittämään ohjelmien virheitä ja koodin suoritusjärjestystä. Se on tärkeä työkalu ohjelmoijille, jotka haluavat varmistaa koodinsa sujuvan ja virheettömän suorituksen.

## Miten

Kun haluat tulostaa debug-tulosteita Ruby-ohjelmassasi, voit käyttää `puts`-metodia. Se tulostaa haluamasi viestin ja mahdollisen muuttujan arvon. Esimerkiksi:

```Ruby
puts "Tulostetaan debug-tuloste!"
puts "Muuttujan arvo on #{muuttuja}"
```

Ohjelman suorituksessa saat seuraavanlaisen tulosteen:

```
Tulostetaan debug-tuloste!
Muuttujan arvo on 10
```

Voit myös käyttää `p`-metodia, joka tulostaa muuttujan arvon sellaisenaan. Esimerkiksi:

```Ruby
muuttuja = 10
p muuttuja
```

Tällöin saat tulosteena `10`.

## Syvemmälle

On tärkeää muistaa, että debug-tulosteiden tulostaminen hidastaa ohjelman suoritusta. Siksi niitä tulisi käyttää vain silloin, kun todella tarvitset tietoa koodin suorituksen vaiheista. Voit myös käyttää ehtolauseita ja käyttää `puts`- tai `p`-metodia vain tietyissä tilanteissa.

Voit myös käyttää `binding.pry`-metodia ohjelman suorituksen keskeyttämiseen ja tarkastella muuttujien arvoja ja suoritusjärjestystä. Tämä on hyödyllinen työkalu monimutkaisempien ohjelmien debuggaamiseen.

## Katso myös

- [Ruby-opetusohjelma](https://www.ruby-lang.org/fi/learn/)
- [Ruby-debuggausopas](https://www.howtogeek.com/howto/6152/use-the-built-in-irb-debugger-to-debug-your-ruby-code/)
- [Ruby-ohjelmoinnin perusteet](https://www.tutorialspoint.com/ruby/ruby_basic_syntax.htm)