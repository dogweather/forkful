---
title:                "Ruby: Komentoriviparametrien lukeminen"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi

Tervetuloa lukemaan tätä Ruby-ohjelmointiblogia, jossa käsitellään miten lukea komentoriviparametreja ja miksi se on tärkeää. ...

## Miten tehdä

Jotta voit lukea komentoriviparametreja Ruby-ohjelmassa, sinun tulee hyödyntää ``ARGV`` -muuttujaa. Tämä muuttuja sisältää tiedot kaikista komentoriviparametreista, jotka on syötetty ohjelman suorittamisen yhteydessä. Käytännön esimerkki näyttää, miten voit käyttää tätä muuttujaa:

```Ruby
# Luo ohjelma, joka tulostaa komentoriviparametrit
puts ARGV
```

Jos ajamme tämän ohjelman käyttäen seuraavaa komentoa:

```
ruby ohjelma.rb foo bar
```

Tulostus olisi:

```
["foo", "bar"]
```

Kuten näet, ARGV-muuttujaan tallennetaan parametrien tiedot taulukkona. Voit myös käyttää ``ARGV`` -muuttujan ``index`` -metodia tarkastellaksesi tiettyjä parametreja tarkemmin. Esimerkiksi:

```Ruby
# Tulostaa toisen komentoriviparametrin
puts ARGV[1]
```

Tässä tapauksessa tulostus olisi:

```
bar
```

## Syvällisempi tarkastelu

Komentoriviparametrien lukeminen on erittäin tärkeä ja hyödyllinen taito Ruby-ohjelmoinnissa. Niiden avulla voit tehdä ohjelmastasi monipuolisemman ja antaa käyttäjille mahdollisuuden välittää tietoja ohjelmalle parametreina. Voit myös suorittaa erilaisia toimintoja riippuen siitä, mitä komentoriviparametreja on annettu. On myös hyödyllistä koodata ohjelma siten, että se hyväksyy erilaisia parametreja ja antaa virheilmoituksen, jos parametreja ei annettu oikeassa muodossa.

## Katso myös

- [Ruby Doc: ARGV](https://ruby-doc.org/core-2.6.3/ARGV.html)
- [RubyLearning: Reading Command-line Arguments](http://rubylearning.com/blog/2011/01/03/reading-command-line-arguments/)