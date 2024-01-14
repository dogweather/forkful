---
title:                "Fish Shell: Työskentely yaml:n kanssa"
simple_title:         "Työskentely yaml:n kanssa"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Miksi

YAML on yksi suosituimmista tapahtumien kirjoitusmuodoista ohjelmistokehittäjien keskuudessa. Se on helppokäyttöinen ja yksinkertainen tapa tallentaa tietoa rakenteelliseen muotoon. YAML: n avulla voit tallentaa tietoja, kuten asetuksia, tietokantoja ja jopa koodin seurantaa.

## Miten

Fish Shell tarjoaa kätevän tavan työskennellä YAML: n kanssa. Voit luoda YAML-tiedostoja Fish Shell-skripteillä ja käyttää niitä sitten ohjelmassa. Voit myös käyttää Fish Shell-komentoja navigoidaksesi YAML-tiedostoissa ja suorittaaksesi niihin muutoksia. Katso esimerkkikoodi alapuolella nähdäksesi, kuinka voit aloittaa YAML-koodauksen Fish Shellillä.

```
# Luodaan YAML-tiedosto nimeltä config.yml ja lisätään siihen tietoa
echo "Nimi: John Doe" > config.yml
echo "Sähköposti: johndoe@example.com" >> config.yml

# Tulostetaan YAML-tiedoston sisältö
cat config.yml

# Muokataan YAML-tiedoston sisältöä
fish -c 'set -Ux Tiedot:nimi "Jane Doe"'
fish -c 'set -Ux Tiedot:sähköposti "janedoe@example.com"'

# Tulostetaan muokattu YAML-tiedosto
cat config.yml
```

Odotettu tulostus on seuraava:

```
Nimi: John Doe
Sähköposti: johndoe@example.com

Nimi: Jane Doe
Sähköposti: janedoe@example.com
```

## Syvemmälle

Fish Shell tarjoaa myös useita komentoja ja toimintoja, joita voit käyttää YAML-tiedostojen kanssa työskennellessäsi. Voit esimerkiksi käyttää `yq` -komentoa muuttaaksesi tiettyjä arvoja YAML-tiedostossa tai `yq eval` -komentoa suorittaaksesi monimutkaisempia tehtäviä ja käsittelyjä.

Ole varovainen käyttäessäsi Fish Shell-komentoja YAML-tiedostoihin, sillä ne voivat helposti muuttaa tai vahingoittaa tietoja. On aina hyvä idea tehdä varmuuskopio YAML-tiedostosta ennen kuin käytät komentoja siihen.

## Katso myös

- [Fish Shellin viralliset sivut](https://fishshell.com/)
- [YAML-perusopas](https://yaml.org/)
- [Fish Shellin asennusohjeet](https://github.com/fish-shell/fish-shell)
- [YAML-komentorivityökalu `yq` käyttöohjeet](https://github.com/mikefarah/yq)