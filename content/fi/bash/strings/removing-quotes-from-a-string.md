---
date: 2024-01-26 03:37:54.779338-07:00
description: "Merkkijonosta lainausmerkkien poistaminen tarkoittaa niiden lainausmerkkien\
  \ karsimista, jotka sulkevat merkkijonon. Ohjelmoijat haluavat usein tehd\xE4 t\xE4\
  m\xE4n\u2026"
lastmod: '2024-03-13T22:44:56.726137-06:00'
model: gpt-4-0125-preview
summary: Merkkijonosta lainausmerkkien poistaminen tarkoittaa niiden lainausmerkkien
  karsimista, jotka sulkevat merkkijonon.
title: Merkkijonosta lainausmerkkien poistaminen
weight: 9
---

## Kuinka:
Bash tarjoaa useita tapoja poistaa lainausmerkit merkkijonoista. Tässä muutamia nopeita esimerkkejä:

```Bash
#!/bin/bash

# Käyttäen muuttujan korvausta sekä yksittäisten että kaksinkertaisten lainausmerkkien poistoon
STRING="\"Hello, World!\""
echo ${STRING//\"}

# Käyttäen `tr`:ää lainausmerkkien poistoon
STRING="'Hello, World!'"
echo $STRING | tr -d "\'"

# Käyttäen `sed`:iä lainausmerkkien poistoon
STRING="\"Hello, World!\""
echo $STRING | sed 's/"//g'
```

Esimerkkitulostus:

```
Hello, World!
Hello, World!
Hello, World!
```

## Syväsukellus
Aikoinaan Unix-komennot kuten `tr` ja `sed` olivat päätensiotekstinkäsittelyyn. Niitä käytetään edelleen nykyään niiden joustavuuden ja tehon vuoksi tekstimuunnoksissa, kuten lainausmerkkien poistamisessa. Ne ovat olennainen osa kenraation kuoriskriptien työkalupakkia.

Bash on itse kehittynyt tästä, ja muuttujan korvaus lisää toisen kerroksen yksinkertaisuutta pienimuotoisiin merkkijonomanipulaatioihin. Se säästää sinua putkittamasta ulkopuolisiin binääreihin, tehden skripteistäsi hieman tehokkaampia.

Vaikka `tr` on hieno merkkien poistamiseen, se ei käsittele monimutkaisempia kuvioita. `sed` toisaalta käyttää säännöllisiä lausekkeita, joten se on joskus liioittelua ja saattaa olla hitaampi yksinkertaisissa operaatioissa.

Menetelmän valinta riippuu tapauksestasi. Jos sinun tarvitsee poistaa erilaisia lainausmerkkejä ja olet jo Bash-skriptin kontekstissa, muuttujan korvausta on vaikea voittaa sen yksinkertaisuuden vuoksi. Mutta jos olet muuntamassa tekstivirtoja tai monirivisiä tietoja, `tr` ja `sed` ovat tukevasi kaverit.

## Katso myös:
- GNU Bash -käsikirja, erityisesti osiot Parametrin Laajennus ja Kuoriparametrin Laajennus: https://www.gnu.org/software/bash/manual/
- `tr`:n komentokäsikirja: https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html
- `sed` virtuaeditorin yleiskatsaus: https://www.gnu.org/software/sed/manual/sed.html
