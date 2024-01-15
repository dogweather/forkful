---
title:                "Verkkosivun lataaminen"
html_title:           "Ruby: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Miksi

Saatat kysyä itseltäsi, miksi haluat ladata verkkosivun ohjelmallisesti Ruby-kielellä. On monia syitä, miksi tämä voisi olla hyödyllistä - voit esimerkiksi haluta kerätä tietoa verkkosivustosta, analysoida sivun sisältöä tai automatisoida tiettyjä tehtäviä, kuten tiedon keräämistä sivuilta.

## Miten

Lataaminen verkkosivulle Ruby-kielellä on melko yksinkertaista. Tässä käytämme Ruby-kirjastoa nimeltä "open-uri", joka mahdollistaa verkkosivujen lataamisen yhdellä rivillä koodia.

```ruby
require "open-uri"

# Tallenna sivun sisältö muuttujaan
page = open("https://www.example.com").read

# Tulosta sivun sisältö konsolille
puts page
```

Tämä esimerkki lataa sivun https://www.example.com ja tallentaa sen sisällön muuttujaan nimeltä "page". Sitten se tulostaa sivun sisällön konsolille komennolla "puts page".

## Syvällinen sukellus

Lisäksi open-uri-kirjastolla on monia vaihtoehtoja, joiden avulla voit muokata ja hallita ladattavaa sisältöä. Voit muun muassa määrittää käyttämäsi URL-osoitteen parametrina, lisätä käyttäjätunnuksen ja salasanan, käyttää erilaisia HTTP-metodeja ja hallita HTTP-pyyntöjen otsikoita.

Tämä esimerkki lataa verkkosivun ja tallentaa sen sisällön tiedostoon nimeltä "page.html":

```ruby
require "open-uri"

# Lataa sivu määriteltyyn tiedostoon
open("https://www.example.com", "w") do |f|
  f.puts open("https://www.example.com").read
end
```

Kuten näet, voit myös käyttää "open"-metodia parametrien kanssa, jotta voit määrittää erityisiä asetuksia pyynnölle.

## Katso myös

- [Open-uri dokumentaatio](https://ruby-doc.org/stdlib/libdoc/open-uri/rdoc/index.html)
- [Ruby-asukassivuston lataaminen ja sisällön käyttö](https://www.rubyguides.com/2018/10/ruby-web-scraping/)

### Disclaimer

Tämä artikkeli on tarkoitettu vain opetus- ja informatiivisiin tarkoituksiin. Muista käyttää verkkosivujen lataamista vastuullisesti ja kunnioittaen verkkosivuston omistajien oikeuksia.