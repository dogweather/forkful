---
title:    "Ruby: Standard errorin kirjoittaminen"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

Monet ohjelmoijat ovat saattaneet törmätä termiin "standard error" Ruby-kielen yhteydessä. Mutta miksi se on tärkeä ja miksi sitä tulisi hyödyntää koodissa? Tässä kirjoituksessa käymme läpi syitä ja hyviä käytäntöjä, miten kirjoittaa standard erroriin Ruby-ohjelmoinnissa.

## Miten

Kirjoittaminen standard erroriin on yksinkertaista Rubyssa. Käytä `STDERR.puts` tai `STDERR.print` -funktioita tulostamaan haluamasi viesti. Esimerkiksi:

```Ruby
STDERR.puts "Tämä on virheviesti"
STDERR.print "Tämä on toinen virheviesti"
```

Tämä tulostaa "Tämä on virheviesti" ja "Tämä on toinen virheviesti" terminaaliin. On myös mahdollista ohjata virheviestit suoraan standard erroriin, esimerkiksi:

```Ruby
2 / 0 # aiheuttaa jakojäännösvirheen
```

Tämä tulostaisi "ZeroDivisionError: divided by 0" standard erroriin.

## Syvemmälle

Nyt kun tiedämme, miten kirjoittaa standard erroriin Rubyssa, on tärkeää ymmärtää, miksi se on hyvä käytäntö. Standard error on erillinen virheviestiloki, joka on erillinen standardilokista. Näin ollen, se erottaa selkeämmin virheviestit normaalista tulostamisesta ja helpottaa ohjelman virheiden löytämistä ja korjaamista.

Kun ohjelmointiin sisältyy käyttäjän syötteen käsittely, käytävien virheiden käsittely on myös tärkeää. Kirjoittaminen standard erroriin mahdollistaa käyttäjän virheiden ohjaamisen selkeämmin ja välittämisen ohjelmoijalle selityksellä. Tämä auttaa myös käyttäjää ymmärtämään, miksi ohjelma ei toimi halutulla tavalla.

## Katso myös

- [Ruby Standard Library Documentation: StandardError](https://ruby-doc.org/core-2.7.2/StandardError.html)
- [How to Use Standard Error in Ruby](https://www.geeksforgeeks.org/how-to-use-standard-error-in-ruby/)
- [Debugging Ruby Programs with Standard Error](https://www.redhat.com/sysadmin/debugging-ruby-standard-error)