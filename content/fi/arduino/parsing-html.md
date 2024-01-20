---
title:                "HTML:n jäsentäminen"
html_title:           "Bash: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

HTML-parsinta on prosessi, jossa HTML-koodista poimitaan tietoa. Ohjelmoijat käyttävät tätä menetelmää tietojen sieppaamiseen, joko omasta koodistaan tai etäpalvelimista.

## Näin se tehdään:

Arduino-koodinäyte HTML-parsinnasta:

```Arduino
#include <HTMLParser.h>

void setup() {
  HTMLParser parser;

  char html[] = "<title>Tämä on koodin otsikko</title>";
  parser.parse(html);

  if (parser.available()) {
    Serial.println(parser.tagName());
  }
}
```

Ajosta saatu tuloste:
```
title
```

Tässä koodissa ladataan `HTMLParser` kirjasto, ja sille annetaan parsittavaksi HTML-rivi. Tulostetaan tagin nimi, joka tässä tapauksessa on "title".

## Syvemmälle

HTML-parsinnan juuret juontavat internetin alkuaikoihin, kun dynaamisen sisällön lukeminen verkkosivuilta tuli tarpeelliseksi. Nykyään on olemassa useita vaihtoehtoisia kirjastoja, kuten htmlcxx, Gumbo ja MyHtml. Tyypillinen HTML-parseri koostuu kahdesta osasta: jäsentimestä, joka lukee HTML-tekstidataa, ja navigaattorista, joka liikkuu luodussa puurakenteessa.

## Katso myös

Valmiita HTML-parsintakirjastoja:
- [htmlcxx](http://htmlcxx.sourceforge.net/)
- [Gumbo](https://github.com/google/gumbo-parser) 
- [MyHtml](https://github.com/lexborisov/MyHTML)

HTML-parsinnasta lisää:
- [Web Scraping with Python](https://realpython.com/beautiful-soup-web-scraper-python/)
- [Beautiful Soup Documentation](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
- [Arduino on Github](https://github.com/arduino/Arduino)