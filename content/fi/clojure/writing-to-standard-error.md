---
title:                "Kirjoittaminen standardivirheelle"
html_title:           "Clojure: Kirjoittaminen standardivirheelle"
simple_title:         "Kirjoittaminen standardivirheelle"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Mitä & Miksi?

Kirjoittaminen standardivirheeseen (standard error) tarkoittaa viestin lähettämistä virheriville, jota ohjelmointikieli käyttää ohjelman virheilmoitusten tulostamiseen käyttäjälle. Tämä on yleinen tapa ohjelmoijille ilmoittaa virheistä, jotka voivat auttaa korjaamaan ja vianetsimään koodia.

# Miten:

```Clojure
;; Koodiesimerkki
(println "Tämä viesti lähetetään standardivirheelle.")
```

```
Tämä viesti lähetetään standardivirheelle.
```

# Syventyvä sukellus:

Standardivirheellä on historiallisia juuria Unix-käyttöjärjestelmästä, jossa se oli tapa ilmoittaa käyttäjälle virheet ja muut tärkeät viestit. Nykyään se on yleisessä käytössä myös muissa ohjelmointikielissä, eikä ole ainoa tapa ilmoittaa virheistä. Esimerkiksi voi myös käyttää standarditulostetta (standard output) tai tallentaa virheet lokitiedostoon.

# Katso myös:

- [Clojure - Virheiden hallinta](https://clojuredocs.org/clojure.core/throw)
- [Unix - Standardivirhe](https://www.computerhope.com/unix/unix-stderr.htm)