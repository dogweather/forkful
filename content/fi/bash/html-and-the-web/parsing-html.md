---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:45.806141-07:00
description: "HTML:n j\xE4sent\xE4minen tarkoittaa HTML-tiedoston rakenteen ja sis\xE4\
  ll\xF6n seulomista tiedon poimimiseksi. Ohjelmoijat tekev\xE4t sit\xE4 p\xE4\xE4\
  st\xE4kseen k\xE4siksi\u2026"
lastmod: '2024-02-25T18:49:53.649066-07:00'
model: gpt-4-0125-preview
summary: "HTML:n j\xE4sent\xE4minen tarkoittaa HTML-tiedoston rakenteen ja sis\xE4\
  ll\xF6n seulomista tiedon poimimiseksi. Ohjelmoijat tekev\xE4t sit\xE4 p\xE4\xE4\
  st\xE4kseen k\xE4siksi\u2026"
title: "HTML:n j\xE4sennys"
---

{{< edit_this_page >}}

## Mikä ja miksi?

HTML:n jäsentäminen tarkoittaa HTML-tiedoston rakenteen ja sisällön seulomista tiedon poimimiseksi. Ohjelmoijat tekevät sitä päästäkseen käsiksi tietoihin, manipuloimaan sisältöä tai raapimaan verkkosivustoja.

## Kuinka:

Bash ei ole ensisijainen työkalu HTML:n jäsentämiseen, mutta se on mahdollista tehdä työkaluilla kuten `grep`, `awk`, `sed` tai ulkoisilla apuohjelmilla kuten `lynx`. Robustisuuden vuoksi käytämme `xmllint`iä `libxml2`-paketista.

```bash
# Asenna xmllint tarvittaessa
sudo apt-get install libxml2-utils

# Esimerkki HTML
cat > sample.html <<EOF
<html>
<head>
  <title>Esimerkkisivu</title>
</head>
<body>
  <h1>Hei, Bash!</h1>
  <p id="myPara">Bash voi lukea minut.</p>
</body>
</html>
EOF

# Jäsennä otsikko
title=$(xmllint --html --xpath '//title/text()' sample.html 2>/dev/null)
echo "Otsikko on: $title"

# Poimi kappale ID:n perusteella
para=$(xmllint --html --xpath '//*[@id="myPara"]/text()' sample.html 2>/dev/null)
echo "Kappaleen sisältö on: $para"
```

Tuloste:
```
Otsikko on: Esimerkkisivu
Kappaleen sisältö on: Bash voi lukea minut.
```

## Syväsukellus

Aikaisemmin ohjelmoijat käyttivät regex-pohjaisia työkaluja kuten `grep` HTML:n skannaamiseen, mutta se oli kömpelöä. HTML ei ole säännönmukaista - se on kontekstuaalista. Perinteiset työkalut eivät ota tätä huomioon ja voivat olla virhealttiita.

Vaihtoehtoja? Runsain mitoin. Python kauniin keiton (Beautiful Soup) kanssa, PHP DOMDocumentin kanssa, JavaScript DOM-jäsentäjien kanssa - kielet, joilla on kirjastoja, jotka on suunniteltu ymmärtämään HTML:n rakennetta.

`xmllint`in käyttö bash-skripteissä on vankkaa yksinkertaisiin tehtäviin. Se ymmärtää XML:n, ja sitä kautta XHTML:n. Säännöllinen HTML voi olla arvaamatonta, kuitenkin. Se ei aina noudata XML:n tiukkoja sääntöjä. `xmllint` pakottaa HTML:n XML-malliin, joka toimii hyvin muodollisesti oikein muotoillun HTML:n kanssa, mutta voi kompastua sotkuisiin tapauksiin.

## Katso myös

- [W3Schools - HTML DOM Jäsentäjä](https://www.w3schools.com/xml/dom_intro.asp): Avaa HTML DOM:n saloja.
- [MDN Web Docs - XML:n jäsentäminen ja serialisointi](https://developer.mozilla.org/en-US/docs/Web/Guide/Parsing_and_serializing_XML): XML:n jäsentämisen periaatteita, jotka pätevät XHTML:ssä.
- [Beautiful Soup -dokumentaatio](https://www.crummy.com/software/BeautifulSoup/bs4/doc/): Python-kirjasto HTML:n jäsentämiseen.
- [libxml2 -dokumentaatio](http://xmlsoft.org/): Tietoja `xmllint`istä ja liittyvistä XML-työkaluista.
