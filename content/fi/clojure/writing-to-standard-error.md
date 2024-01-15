---
title:                "Kirjoitus standardivirheeseen"
html_title:           "Clojure: Kirjoitus standardivirheeseen"
simple_title:         "Kirjoitus standardivirheeseen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

Miksi kirjoittaa standardivirheille? Yksi syy voisi olla virheiden parempi hallinta ohjelmassa.

## Miten

```Clojure
(println :standardivirhe "Tämä on virhe!")
```
Tämä koodi tulostaa "standardivirhe Tämä on virhe!" standardivirheelle. Tästä syystä se näkyy käyttäjän näytöllä tai tallentuu lokitiedostoon.

## Syvällisempi sukellus

Kirjoittaminen standardivirheille on hyödyllistä, jos haluat erottaa virheilmoitukset muusta tulostuksesta. Voit myös käyttää `eprintln`-funktiota, joka tulostaa koko virheen ilman ylimääräisiä tekstirivejä.

# Katso myös

- [Ohjeet standardivirheiden käsittelystä Clojuressa](https://clojure.org/guides/err)  
- [Virheiden hallinta Clojuressa](https://clojure.org/reference/exceptions)