---
title:                "HTML-analysointi"
html_title:           "Kotlin: HTML-analysointi"
simple_title:         "HTML-analysointi"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/parsing-html.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
HTML-analyysi on prosessi, jossa ohjelmoijat muuttavat verkkosivun koodin helposti luettavaan muotoon. Tämä on tärkeää, koska se mahdollistaa tiedon keräämisen ja manipuloinnin verkkosivuilta. Tämä helpottaa monia verkkoon liittyviä tehtäviä, kuten tietojen tallentamista tai hakemista.

## Miten:
```Kotlin 
val html = "<html><body><h1>Hello, world!</h1></body></html>"
val parsedHtml = Jsoup.parse(html)
val title = parsedHtml.select("h1").text()
println(title)
```

Tämä esimerkki tulostaa "Hello, world!" selkeästi ilman HTML-tageja. Käytämme tässä Jsoup-kirjastoa, joka on suosittu HTML-analyysiin tarkoitettu kirjasto.

## Syväsukellus:
HTML-analyysi on ollut tärkeä osa verkkokehitystä jo vuosikymmenten ajan. Alun perin sitä käytettiin pääasiassa verkkosivustojen suunnittelussa ja testaamisessa, mutta nykyään sillä on paljon muita käyttötarkoituksia. On myös muita kirjastoja, kuten järjestelmään sisltyvä org.xml.sax, jotka voivat auttaa HTML-analyysissä.

## Katso myös:
- Jsoup-kirjaston viralliset dokumentaatiot: https://jsoup.org/
- Orkxml.sax: https://docs.oracle.com/javase/7/docs/api/org/xml/sax/package-summary.html