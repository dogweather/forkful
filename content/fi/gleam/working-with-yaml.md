---
title:                "Työskentely yaml:n kanssa"
html_title:           "Gleam: Työskentely yaml:n kanssa"
simple_title:         "Työskentely yaml:n kanssa"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

Mikä & miksi?

Gleam on ohjelmointikieli, joka mahdollistaa YAML-tiedostojen käsittelyn. YAML on eräänlainen tiedostomuoto, joka on suunniteltu ihmisluettavaksi ja helposti käsiteltäväksi myös ohjelmointikielillä. Monet ohjelmoijat käyttävät YAML-tiedostoja, koska ne ovat selkeitä ja helppoja ymmärtää.

Kuinka tehdä:

Gleamilla on helppo lukea ja kirjoittaa YAML-tiedostoja. Voit esimerkiksi luoda uuden YAML-tiedoston ja lisätä siihen haluamasi tiedot käyttämällä ```Gleam.from_yml("tiedoston_nimi.yml")``` komentoa. Voit myös muuntaa YAML-tiedoston Gleam-taulukoksi käyttämällä ```Gleam.to_map(tiedoston_nimi.yml)``` komentoa. Alla on esimerkkejä koodista ja tulostatuloksista:

```
Gleam.from_yml("kayttaja.yml")

käyttäjä: "Matti"
ikä: 30
sähköposti: "matti@example.com"

```

Tulostettu tulos olisi taulukko, jossa käyttäjän nimi on "Matti", ikä 30 ja sähköpostiosoite "matti@example.com".

Syvemmälle:

YAML on alunperin luotu yhdistämään eri ohjelmointikielten siirtymä- ja tallennusformaattia. YAML-tiedostot ovat yleistymässä, sillä ne ovat ihmisen luettavissa ja helposti ymmärrettävissä. Muiden ohjelmointikielien lisäksi, on olemassa myös muita vaihtoehtoja YAML:lle, kuten XML ja JSON. Gleamin ansiosta voit kuitenkin käsitellä YAML-tiedostoja monimutkaisemmin ja tehokkaammin.

Lisätietoja:

Voit lukea lisää Gleamista ja YAML-tiedostoista täältä: [Gleamin käsikirja](docs.gleamlang.org), [YAML.org](yaml.org), [XML vs JSON vs YAML](https://www.educba.com/xml-vs-json-vs-yaml/) ja [Osaohjelmointikielten vertailu](https://www.xml.com/pub/a/2004/07/21/json.html).

Tämän artikkelin tarkoituksena ei ole olla kattava opas Gleamin UART-kehitykselle, mutta toivomme sen antavan sinulle hyvän lähtökohdan aloittamiseen YAML-tiedostojen käsittelyssä Gleamilla. Onnea matkaan!