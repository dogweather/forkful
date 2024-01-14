---
title:    "Ruby: Väliaikaistiedoston luominen"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

--

## Miksi

Monet Ruby-kehittäjät tarvitsevat väliaikaisia tiedostoja ohjelman suorittamisen aikana. Tämä voi johtua tarpeesta tallentaa väliaikaisia tietoja tai työstää suuria määriä tietoa, jotka on helpointa käsitellä yhden tiedoston kautta. Ruby tarjoaa helpon tavan luoda väliaikaisia tiedostoja, jotka voidaan poistaa ohjelman suorituksen päätyttyä.

## Miten

Väliaikaisen tiedoston luominen Rubyssa on yksinkertaista. Käytetään `Tempfile`-luokkaa ja kutsutaan `open`-metodia antaen sille parametriksi tiedoston nimi ja tiedostomuoto.

```Ruby
file = Tempfile.open(["tempfile", ".txt"])
```

Tämä luo uuden väliaikaisen tiedoston nimellä "tempfile" ja tiedostopäätteellä ".txt". Tiedosto tallentuu automaattisesti Ruby-ohjelman oletushakemistoon. Voit myös määrittää tietyn hakemiston `dir`-parametrillä.

```Ruby
file = Tempfile.open(["tempfile", ".txt"], dir: "~/Documents")
```

Väliaikainen tiedosto voidaan nyt käsitellä kuten mitä tahansa muuta tiedostoa Rubyssa. Alla on esimerkki, jossa tiedostoon kirjoitetaan ja siitä luetaan tekstiä.

```Ruby
file.write("Tämä on väliaikainen tiedosto.")
file.rewind
file.read #=> "Tämä on väliaikainen tiedosto."
```

Kun tiedostoa ei enää tarvita, se voidaan poistaa kutsuttaessa `close`-metodia.

```Ruby
file.close
```

## Syvällinen sukellus

`Tempfile`-luokka pohjautuu `File`-luokkaan ja tarjoaa lisäominaisuuksia väliaikaisen tiedoston hallintaan. Voit esimerkiksi määrittää tiedoston käyttöoikeudet `perms`-parametrilla ja määrittää tiedoston avaustilan `mode`-parametrilla.

```Ruby
file = Tempfile.open(["tempfile", ".txt"], perms: 0644, mode: "w+")
```

`Tempfile`-luokassa on myös metodi `unlink`, joka poistaa tiedoston automaattisesti, kun tiedosto suljetaan. Tämä on kätevä tapa varmistaa, ettei väliaikaisia tiedostoja jää turhaan levylle.

Lisätietoa `Tempfile`-luokasta ja sen ominaisuuksista löytyy [Ruby-dokumentaatiosta](https://ruby-doc.org/stdlib-2.6.3/libdoc/tempfile/rdoc/Tempfile.html).

## Katso myös

- [Ruby-dokumentaatio `Tempfile`-luokasta](https://ruby-doc.org/stdlib-2.6.3/libdoc/tempfile/rdoc/Tempfile.html)
- [Artikkeli: "Temporary files in Ruby: a comprehensive guide"](https://www.honeybadger.io/blog/temporary-files-in-ruby-a-comprehensive-guide/)