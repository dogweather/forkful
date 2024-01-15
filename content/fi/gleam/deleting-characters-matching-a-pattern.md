---
title:                "Pilkkujen poistaminen vastaavan kuvion avulla"
html_title:           "Gleam: Pilkkujen poistaminen vastaavan kuvion avulla"
simple_title:         "Pilkkujen poistaminen vastaavan kuvion avulla"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Kaikki ohjelmoijat joutuvat joskus käsittelemään tekstiä ja poistamaan siitä tiettyjä merkkejä. Onneksi Gleamilla on yksinkertainen ja tehokas tapa tehdä tämä! Tässä artikkelissa opit, miten voit käyttää Gleamin sisäänrakennettua toimintoa poistaaksesi merkkejä, jotka vastaavat tiettyä kuvioita.

## Miten

Aluksi sinun tulee määrittää kirjasto `gleam/re` ja lisätä se projektiisi `apps`-kansiossa sijaitsevaan `gleam.toml`-tiedostoon:

```
[dependencies]
gleam = "0.7.0"
gleam/re = "0.6.0"
```

Seuraavaksi sinun tulee tuoda `gleam/re` kirjasto käytettäväksi tiedostossa, jossa aiot käyttää merkkienpoistoa:

```Gleam
import gleam/re
```

Nyt voit käyttää `gleam/re` kirjaston `replace`-toimintoa poistaaksesi merkkejä, jotka vastaavat tiettyä kuvioita. Tässä esimerkissä poistamme kaikki välilyönnit merkkijonosta `Hello World`:

```Gleam
let str = "Hello World"

let new_str = re.replace(str, " ", "")

// new_str: "HelloWorld"
```

Voit myös käyttää säännöllisiä lausekkeita poistaaksesi erityyppisiä merkkejä. Esimerkiksi seuraava kuvio poistaa kaikki numerot merkkijonosta:

```Gleam
let str = "I have 3 cats and 2 dogs"

let new_str = re.replace(str, "\\d", "")

// new_str: "I have  cats and  dogs"
```

Jos haluat poistaa merkkejä, jotka eivät vastaa tiettyjä kuvioita, voit käyttää `negate`-toimintoa yhdessä `replace`-toiminnon kanssa. Esimerkiksi seuraava kuvio poistaa kaikki merkkipisteet merkkijonosta:

```Gleam
let str = "This is a sentence."

let new_str = re.replace(str, re.negate("."), "")

// new_str: "This is a sentence"
```

## Syvempää sukellusta

`replace`-toiminto hyödyntää säännöllisiä lausekkeita poistamaan merkkejä, ja sen avulla voit käyttää monipuolisia kuvioita merkkien poistamiseen. Voit lukea lisää erilaisten säännöllisten lausekkeiden syntaksista ja käyttämisestä Gleamin dokumentaatiosta.

## Katso myös

- Gleam dokumentaatio: https://gleam.run/
- Gleam/re kirjasto: https://hexdocs.pm/re/readme.html
- Säännöllisten lausekkeiden opas: https://www.regular-expressions.info/