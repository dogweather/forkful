---
title:                "Ruby: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## Miksi

Jos olet koskaan selattu verkkosivuja, olet todennäköisesti törmännyt koodattuihin HTML-elementteihin, jotka määrittävät sivun rakenteen ja sisällön. Tiedät myös, että nämä elementit eivät näy sivustolla selkeästi, vaan ne on peitetty muun sisällön alle. Näin ollen, jos haluat saada käsiisi tiettyjä tietoja verkkosivuilta, sinun täytyy "parsia" eli käsitellä HTML-koodia saadaksesi haluamasi tiedot. Tämä on tärkeä taito, kun työskentelet web-kehityksen tai web-skrapingin parissa.

## Kuinka tehdä se

Onneksi, Rubylla on sisäänrakennettu Nokogiri-kirjasto, joka tekee HTML-koodin parsimisen todella helpoksi. Nokogiri tarjoaa valmiit toiminnallisuudet parsimiseen, joten sinun tarvitsee vain ladata kirjasto, luoda uusi Nokogiri-olio ja syöttää siihen haluamasi HTML-koodi. Käytetään esimerkkinä verkkosivulta "https://www.ruby-lang.org/en/" otsikoiden parsimista:

```Ruby
require 'nokogiri' # ladataan Nokogiri-kirjasto
require 'open-uri' # ladataan open-uri-kirjasto, joka auttaa avaamaan URL-osoitteita

page = Nokogiri::HTML(open("https://www.ruby-lang.org/en/")) # luodaan uusi Nokogiri-olio ja annetaan sille haluttu verkkosivu URL-osoitteena
puts page.css("h2").first.text # tulostetaan sivun ensimmäinen otsikko
puts page.css("h2")[1].text # tulostetaan sivun toinen otsikko
```

Tämä koodi luo Nokogiri-olion, jonka avulla pystymme tarkastelemaan verkkosivun rakennetta ja etsimään haluttuja elementtejä CSS-selectorien avulla. Tässä tapauksessa hakukriteerinä oli "h2", mikä tarkoittaa, että haluamme etsiä kaikki HTML-elementit, jotka ovat otsikkoja. Tulostuksena saadaan verkkosivun kaksi ensimmäistä otsikkoa, eli "Ruby 3.0.0 Released" ja "Featured news".

## Syvällisempi tarkastelu

Kuten näemme, parsiminen on melko suoraviivaista Nokogiri-kirjaston avulla. Mutta miten se toimii taustalla? Nokogiri käyttää taustalla libxml-kirjastoa, joka auttaa parsimaan ja analysoimaan HTML-koodia. Nokogirilla on myös muita toimintoja, kuten XPath ja XSLT, jotka tarjoavat tarkempia hakukriteerejä ja mahdollistavat muokkaamaan ja muuntamaan HTML-koodia.

On myös tärkeää huomata, että HTML on hyvin monimuotoista ja siksi parsiminen voi olla haastavaa joissakin tapauksissa. Et välttämättä saa kaikkia haluamiasi tietoja yhdellä koodinpätkällä, vaan saatat joutua testaamaan eri hakukriteerejä ja -toimintoja löytääksesi haluamasi tiedot. Mutta kun olet ymmärtänyt HTML:n rakenteen ja Nokogirin toiminnallisuudet, parsiminen tulee helpommaksi ja voit hyödyntää sitä erilaisissa projekteissa.

## Katso myös

Tässä artikkelissa käsiteltiin vain pintaa parsimisesta ja Nokogirin käytöstä. Jos