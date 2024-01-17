---
title:                "Tulostamisen virheenetsintä"
html_title:           "Gleam: Tulostamisen virheenetsintä"
simple_title:         "Tulostamisen virheenetsintä"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Debug-tulostuksen printtaaminen tarkoittaa koodirivin lisäämistä ohjelmaan, jotta sen suoritusvaiheet ja mahdolliset virheet voi tarkistaa helpommin. Kehittäjät tekevät tätä ohjelman vianmäärityksen helpottamiseksi ja korjaamiseksi.

## Miten:

```Gleam
// Koodiesimerkki 1
test := "Hello World!"
debug!(test)
```

Tämä lisää test-muuttujan debug-tulostuksen koodiin, mikä auttaa kehittäjää varmistamaan, että muuttujan arvo on oikea. Tulostus näkyy ohjelman suoritusta seuratessa.

```Gleam
// Koodiesimerkki 2
for x in [1, 2, 3] {
  debug!("x:n arvo on {}", x)
}
```

Tämä esimerkki käyttää debug-makroa silmukan sisällä, jolloin kehittäjä voi nähdä jokaisen x:n arvon silmukan suorituksen aikana.

## Syvemmälle:

Debug-tulostuksen käyttö on yleinen tapa vianmäärityksessä ja se on ollut osa ohjelmoinnissa jo pitkään. Muita tapoja tarkistaa koodin suoritusta ovat esimerkiksi koodin ajaminen vaiheittain ja breakpointtien käyttö.

Gleamin lisäksi muut ohjelmointikielet tarjoavat myös vastaavia debug-toimintoja, esimerkiksi Pythonin "print" ja JavaScriptin "console.log". Gleamissa debug-tulostuksen toteutus on tehty makron avulla, jotta se olisi tehokas ja helppokäyttöinen.

## Katso myös:

Gleam-dokumentaatio debug-makron käytöstä: https://gleam.run/book/core-modules-debug.html