---
title:                "Komentoriviparametrien lukeminen"
html_title:           "Ruby: Komentoriviparametrien lukeminen"
simple_title:         "Komentoriviparametrien lukeminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Komentoriviparametrien lukeminen on prosessi, jossa ruby-ohjelma lukee käyttäjän syöttämät komennot terminaalin komentoriviltä. Tämä on hyödyllistä, sillä se antaa ohjelmoijille mahdollisuuden antaa dynaamisia komentoja ohjelmalleen ja muokata sen toimintaa haluamallaan tavalla.

## Miten:
Esimerkiksi, jos haluat ohjelmassasi tulostaa käyttäjän antaman nimen, voit käyttää Ruby-metodia ARGV, joka lukee komentoriviltä annetut parametrit ja tallentaa ne taulukkoon. Tämän jälkeen voit käyttää taulukkoa ja sen sisältämiä parametrejä haluamallasi tavalla.

```Ruby
# Esimerkki koodista
puts "Hei, " + ARGV[0] + "!" 
```
Syötetyllä nimellä korvataan ARGV[0] ja ohjelma tulostaa esimerkiksi "Hei, Timo!". Tämän lisäksi voit myös käyttää välilyöntiä sekä muita string-metodeja parametrien välissä ja muokata tulostetta haluamallasi tavalla.

## Syväsukellus:
Komentoriviparametrien lukemista on käytetty jo vuosien ajan ohjelmoinnissa. Se on yksinkertainen, mutta tehokas tapa kommunikoida käyttäjän kanssa ja hallita ohjelman suoritusta. Kuitenkin tätä nykyä on myös muita tapoja lukea syötteitä, kuten esimerkiksi käyttäjän antamaa tietoa ohjelman suorituksen aikana.

## Muuta:
Lisätietoja komentoriviparametrien lukemisesta voit löytää Ruby-ohjelmoinnin oppaista ja dokumentaatiosta.

## Katso myös:
- [Ruby-dokumentaatio](https://www.ruby-lang.org/en/documentation/)
- [Komentoriviparametrien lukeminen Perl-ohjelmoinnissa](https://perlmaven.com/command-line-arguments-in-perl)