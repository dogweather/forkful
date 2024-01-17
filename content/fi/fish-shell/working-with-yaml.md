---
title:                "Työskentely yaml:n kanssa."
html_title:           "Fish Shell: Työskentely yaml:n kanssa."
simple_title:         "Työskentely yaml:n kanssa."
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

### Mitä & Miksi?
YAML eli "Yet Another Markup Language" on ohjelmointikieli, jota käytetään tiedostojen rakenteiden kuvaukseen. Koodaajat käyttävät sitä esimerkiksi konfiguraatiotiedostojen luomiseen.

### Miten:
Fish Shell -ohjelmointikielellä voit helposti työskennellä YAML-tiedostojen kanssa. Katso esimerkit alla olevista koodilohkoista ja niiden tuottamaa tulostetta.

```Fish Shell
# Esimerkki tekstin lisäämisestä YAML-tiedostoon
yq e '.foo.bar = "hello"' example.yml
#Tulostaa:
foo:
  bar: hello
```

```Fish Shell
# Esimerkki muuttujan lisäämisestä YAML-tiedostoon
yq e '.foo.bar = 123' example.yml
#Tulostaa:
foo:
  bar: 123
```

```Fish Shell
# Esimerkki tietojen hakuun YAML-tiedostosta
yq e '.foo.bar' example.yml
#Tulostaa:
hello
```

### Syvällinen sukellus:
YAML-kieli kehitettiin vuonna 2001 painostuksesta, joka kohdistui XML:n vaikeaan käyttöönottoon. Joitakin vaihtoehtoja YAML:lle ovat esimerkiksi JSON ja TOML. Fish Shell käyttää [yq](https://github.com/kislyuk/yq) -työkalua YAML-tiedostojen käsittelyyn.

### Katso myös:
- [Fish Shell ohjeet](https://fishshell.com/docs/current)
- [YAML dokumentaatio](https://yaml.org/spec/)
- [yq työkalun dokumentaatio](https://github.com/kislyuk/yq)