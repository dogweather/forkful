---
title:    "Ruby: Komentoriviparametrien lukeminen"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Miksi: Miksi haluat lukea komentoriviparametreja?

Yksi syy voi olla, että haluat luoda interaktiivisen ohjelman, joka ottaa käyttäjän antamat parametrit ja käsittelee niitä eri tavalla. Komentoriviparametrit voivat myös olla hyödyllisiä ohjelman suorittamisessa tiettyjen asetusten tai vaihtoehtojen kanssa.

## Kuinka: Esimerkkejä komentoriviparametrien lukemisesta

### Perusmuoto

Käytä `ARGV`-muuttujaa saadaksesi kaikki komentoriviparametrit taulukkona.

```Ruby
names = ARGV # ["Jussi", "Mikko", "Anna"]
puts "Hei #{names[0]}, #{names[1]} ja #{names[2]}!"
```

Esimerkissä tulostetaan tervehdys käyttäjille, joiden nimet ovat annettuina komentorivillä.

### Parametrien määrä

Voit myös tarkistaa, kuinka monta parametria käyttäjä on antanut komentorivillä.

```Ruby
puts "Annoit #{ARGV.length} parametria"
```

### Numeroiden käsittely

Voit myös muuntaa parametrit numeromuotoon ja käsitellä niitä laskutoimituksilla.

```Ruby
numbers = ARGV.map(&:to_i) # ["2", 5, "6.4"] => [2, 5, 6]
puts "Ensimmäinen numero on #{numbers[0]}"
puts "#{numbers[1]} + #{numbers[2]} = #{numbers[1] + numbers[2]}"
```

Parametrit voivat olla myös eri tyyppejä, joten niiden muuntaminen voi olla tarpeellista, jotta ne voidaan käsitellä oikein.

## Syvädyli: Syvempää tietoa komentoriviparametrien lukemisesta

Kun suoritat Ruby-ohjelmaa komentoriviltä, kaikki komentorivillä annetut sanat ja välilyönnit tallennetaan `ARGV`-taulukkoon. Voit käsitellä näitä parametreja kuten mikä tahansa muukin taulukko Ruby-valmiuksilla.

Jos haluat lukea komentoriviparametreja ohjelman ulkopuolelta, voit käyttää `gets`-metodia ja `chomp`-metodia poistamaan uuden rivin merkin lopusta.

```Ruby
puts "Kirjoita jotain:"
input = gets.chomp
puts "Syötit: #{input}"
```

`chomp`-metodi poistaa myös kaikki muut rivinvaihtomerkit, joten voit käsitellä syöteparametreja helposti ilman ylimääräisiä välilyöntejä tai merkkejä.

## Katso myös

- [Ruby's ARGV documentation](https://ruby-doc.org/core-2.7.2/ARGV.html)
- [Command Line Arguments in Ruby](https://www.rubyguides.com/2018/05/ruby-command-line-arguments/)
- [Ruby's gets method documentation](https://ruby-doc.org/core-2.7.2/Kernel.html#method-i-gets)