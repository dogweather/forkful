---
title:                "Työskentely jsonin kanssa"
html_title:           "Bash: Työskentely jsonin kanssa"
simple_title:         "Työskentely jsonin kanssa"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/working-with-json.md"
---

{{< edit_this_page >}}

## Miksi

Ehkä olet kuullut JSON:ista, mutta et ole varmaan miksi se on tärkeä osa Bash-ohjelmointia. JSON on tietojen siirtomuoto, joka tekee datan käsittelystä yksinkertaista ja tehokasta. Se on erityisen hyödyllinen, kun halutaan vaihtaa tietoa Web-sovellusten välillä.

## Miten

Käyttäen `jq` työkalua ja muutamia Bash-komentoja, voit helposti käsitellä JSON-tiedostoja. Tässä on esimerkki, jossa haetaan tietoja Stack Exchange API:sta ja tallennetaan ne JSON-tiedostoon.

```Bash
# Asetetaan muuttujaan API URL
url="https://api.stackexchange.com/2.2/users?site=stackoverflow&pagesize=3"

# Käyttäen curl komentoa haetaan API:sta ja tallennetaan vastaus
response=$(curl -s -H "Accept:application/json" "$url")

# Käytetään jq työkalua muokkaamaan JSON-tiedoston ulkoasua
echo "$response" | jq '.'

# Tulostaa:
# {
#  "items": [
#    {
#      "account_id": 6176949,
#      "is_employee": false,
#      "last_modified_date": 1617897245,
#      "display_name": "Katri",
#      "profile_image": "https://www.gravatar.com/avatar/7ca399b7bc847fe69e377d5764c92aad?s=128&d=identicon&r=PG&f=1"
#    },
#    {
#      "account_id": 6109350,
#      "is_employee": false,
#      "last_modified_date": 1617896697,
#      "display_name": "Matti",
#      "profile_image": "https://www.gravatar.com/avatar/3c6cec8609641957445d810202f3a87f?s=128&d=identicon&r=PG&f=1"
#    },
#    {
#      "account_id": 6181036,
#      "is_employee": false,
#      "last_modified_date": 1617806196,
#      "display_name": "Liisa",
#      "profile_image": "https://www.gravatar.com/avatar/cc9c61be404244b583da0a719ff89954?s=128&d=identicon&r=PG&f=1"
#    }
#  ],
#  "has_more": true,
#  "quota_max": 300,
#  "quota_remaining": 297
#}

# Tallennetaan vastaus tiedostoon
echo "$response" > users.json
```

## Syvemmälle

JSON-tiedostojen käsittelyyn on monia muita Bash-komentoja ja työkaluja, kuten `sed`, `awk` ja `jshon`. Voit myös käyttää Bash-skriptejä automatisoimaan JSON-tiedostojen muokkausta ja käsittelyä.

## Katso myös

- [jq dokumentaatio](https://stedolan.github.io/jq/)
- [Bash-skriptien JSON-käsittely](https://medium.com/@warebot/bash-scripting-and-json-parsing-419beffab82a)
- [JSON-tiedoston luominen Bash-komennoilla](https://www.geeksforgeeks.org/creating-json-file-using-bash/)