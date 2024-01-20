---
title:                "HTML:n jäsentäminen"
html_title:           "Bash: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/parsing-html.md"
---

{{< edit_this_page >}}

## Mitä & Miksi? 

HTML:n jäsentäminen tarkoittaa HTML-koodin halkomista yksittäisiksi osiksi (tagit, attribuutit ja muut), jotta voimme käsitellä niitä erikseen. Ohjelmoijat tekevät tämän yleensä tiedon keräämiseksi tai sisällön muokkaamiseksi.

## Miten:

Jäsentämiseen HTML:a Bash:ssa voimme käyttää työkaluja kuten 'grep', 'sed' and 'awk'. Tässä on esimerkki:

```Bash
#!/bin/bash
# Etsi kaikki 'h1' tagit
curl -s https://esimerkki.fi | grep -o '<h1[^>]*>.*</h1>' 
```
Xuutta ajetaan, se tulostaa kaikki 'h1' tagi:t esimerkki.fi sivulta.

## Sukellus syvemmälle:

HTML:n jäsentäminen ilmestyi ensimmäiseksi 1980-luvulla, kun WWW:ssä käytettiin ensisijaisesti HTML:ää tiedon merkkaamiseen. Nykyään on olemassa monia vaihtoehtoja, kuten XML ja JSON, mutta HTML pysyy yhä standardina.

Vaikka se onkin mahdollista Bash:ssa, HTML:n jäsentäminen Bash:ssa ei ole aina soveliasta, koska Bash ei pysty käsittelemään monimutkaisia HTML rakenteita yhtä tehokkaasti kuin jotkin erikoistuneet työkalut.

Työkalujen, kuten 'grep', 'sed' and 'awk', rajoitukset tulevat esille kun käsitellään monimutkaisempia HTML rakenteita. Tässä tapauksessa kannattaa harkita erikoistuneempien työkalujen, kuten 'Beautiful Soup' (Python pohjainen) tai 'jsoup' (Java pohjainen), käyttöä.

## Katso myös:

1. ['Beautiful Soup' Dokumentaatio](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
1. ['jsoup' Dokumentaatio](https://jsoup.org/)
1. [W3Schools HTML Tutorial](https://www.w3schools.com/html/)
1. [Bash-skriptausohje](https://www.shellscript.sh/)