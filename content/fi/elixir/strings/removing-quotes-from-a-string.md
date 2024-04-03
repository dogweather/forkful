---
date: 2024-01-26 03:38:44.138035-07:00
description: "Kuinka: Elixiriss\xE4 ei ole sis\xE4\xE4nrakennettua 'lainausmerkkien\
  \ poisto' -toimintoa, mutta on helppoa luoda oma ratkaisu k\xE4ytt\xE4m\xE4ll\xE4\
  \ mallin sopivuutta tai\u2026"
lastmod: '2024-03-13T22:44:56.214501-06:00'
model: gpt-4-0125-preview
summary: "Elixiriss\xE4 ei ole sis\xE4\xE4nrakennettua 'lainausmerkkien poisto' -toimintoa,\
  \ mutta on helppoa luoda oma ratkaisu k\xE4ytt\xE4m\xE4ll\xE4 mallin sopivuutta\
  \ tai `String`-funktioita."
title: Merkkijonosta lainausmerkkien poistaminen
weight: 9
---

## Kuinka:
Elixirissä ei ole sisäänrakennettua 'lainausmerkkien poisto' -toimintoa, mutta on helppoa luoda oma ratkaisu käyttämällä mallin sopivuutta tai `String`-funktioita. Katso nämä pätkät:

```elixir
# Käyttäen mallin sopivuutta
def unquote_string("\"" <> quoted_string <> "\""), do: quoted_string
def unquote_string("'" <> quoted_string <> "'"), do: quoted_string
def unquote_string(quoted_string), do: quoted_string

# Esimerkki käytöstä
unquote_string("\"Hei, Maailma!\"") # => "Hei, Maailma!"
unquote_string("'Hei, Maailma!'")   # => "Hei, Maailma!"

# Käyttäen String.trim/1
def unquote_string(string), do: String.trim(string, "'\"")

# Esimerkki käytöstä
unquote_string("\"Hei, Maailma!\"") # => "Hei, Maailma!"
unquote_string("'Hei, Maailma!'")   # => "Hei, Maailma!"
```

Molemmat menetelmät tuottavat:
```
"Hei, Maailma!"
```

## Syväsukellus
Aikoinaan lainausmerkit merkkijonoissa olivat miinakenttä—käsittele niitä väärin, ja pam, syntaksivirheitä tai turvallisuusaukkoja. Elixirissa mallin sopivuus kohtelee merkkijonojasi kuin Lego-palikoita, antaen sinun purkaa ja rakentaa niitä tarkasti. Sen robusti `String`-moduuli on myös kätevä, joustavasti poistaen lainausmerkkejä `trim`-funktioiden avulla. Vaihtoehdot? Säännölliset lausekkeet voivat potkaista lainausmerkit pois tieltä, ja ulkoiset kirjastot saattavat tarjota lisätehoa, jos tarvitset enemmän kuin perusstrippausta.

## Katso Myös
Sukella syvemmälle näiden avulla:
- [Elixirin String-moduuli](https://hexdocs.pm/elixir/String.html)
- [Opi lisää mallin sopivuudesta Elixirissä](https://elixir-lang.org/getting-started/pattern-matching.html)
- [Säännölliset lausekkeet Elixirissä (Regex-moduuli)](https://hexdocs.pm/elixir/Regex.html)
