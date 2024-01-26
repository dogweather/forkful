---
title:                "YAML-tiedostojen käsittely"
html_title:           "Arduino: YAML-tiedostojen käsittely"
simple_title:         "YAML-tiedostojen käsittely"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
"Mikä & Miksi?"

YAML on datan esittämiseen käytetty formaatti, selkeästi luettava ja ymmärrettävä. Käytämme YAMLia, koska se on ihmisystävällinen, ja soveltuu hyvin asetusten, konfiguraatioiden sekä tiedon tallentamiseen ja siirtämiseen.

## How to:
"Miten tehdään:"

YAML-tiedostojen käsittely Fish Shell:ssä vaatii ulkoisen työkalun, kuten 'yq'. Asenna ensin 'yq' käyttäen esim. 'brew':

```Fish Shell
brew install yq
```

Lue YAML-tiedosto ja muuta sisältöä:

```Fish Shell
# Lue arvo
yq e '.someKey' config.yaml

# Muuta arvoa ja tallenna
yq e '.someKey = "new value"' -i config.yaml
```

Lisää uusi avain-arvopari:

```Fish Shell
yq e '.newKey = "new value"' -i config.yaml
```

Tulostus tulee olemaan YAML-muodossa, eli esimerkiksi:

```
someKey: new value
newKey: new value
```

## Deep Dive
"Sukellus syvemmälle"

YAML (YAML Ain't Markup Language) on luotu alun perin 2001, ja on kehittynyt yleisesti käytetyksi konfiguraatiokielenä. 'yq' on suosittu komentorivityökalu, joka käyttää 'jq':n (JSON-tiedostoille) tapaan laajaa kysely- ja muokkausominaisuutta YAML-tiedostoille. Vaihtoehtoja 'yq':lle ovat esimerkiksi 'Pyyaml' Pythonille tai 'RbYAML' Rubylle. Tehokkuus syntyy 'yq':n kyvystä lukea ja kirjoittaa YAML-dataa suoraviivaisesti komentoriviltä.

## See Also
"Katso myös"

- YAML:n kotisivu: [https://yaml.org/](https://yaml.org/)
- 'yq' GitHub-sivu: [https://github.com/mikefarah/yq](https://github.com/mikefarah/yq)
- Fish Shell dokumentaatio: [https://fishshell.com/docs/current/](https://fishshell.com/docs/current/)
- YAML validatori (Verkko-työkalu YAML-tiedostojen validoimiseksi): [http://www.yamllint.com/](http://www.yamllint.com/)
