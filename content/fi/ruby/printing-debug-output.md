---
title:                "Tulostamalla virheenkorjaustulosteita"
html_title:           "Ruby: Tulostamalla virheenkorjaustulosteita"
simple_title:         "Tulostamalla virheenkorjaustulosteita"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Printtaaminen debug-tulostustietoja on yksinkertainen tapa nähdä koodin suorituksen aikaisia tietoja ohjelman suorittamisen aikana. Kehittäjät käyttävät tätä menetelmää löytämään ja korjaamaan virheitä koodin sisällä.

## Miten:
Ruby-kieli tarjoaa useita tapoja tulostaa debug-tulostustietoja. Yksinkertaisin tapa on käyttää `print` tai `puts` -komennolla. Tämä tulostaa halutut tiedot terminaaliin. Esimerkiksi:

```Ruby
age = 24
puts "Olen #{age} -vuotias."
```
Tulostaa: "Olen 24-vuotias."

## Syvempi sukellus:
Debug-tulostus ei ole uusi keksintö. Ennen modernia tekniikkaa, ohjelmoijat käyttivät tulostuspaperia saadakseen tietoa ohjelman suorituksen aikaisista virheistä. Nykyään on olemassa myös muita keinoja kuten debugger-työkalut, mutta tulostaminen on edelleen nopein ja helpoin tapa löytää ongelmia koodista. 

## Katso myös:
Ilman debug-tulostusta monien virheiden löytäminen ja korjaaminen voi olla kovin haastavaa. Joten, muista pienenä koodarivinkkinä seuraava kerta kun kohtaat ongelman koodissa: käytä `print` tai `puts` ja tulosta koodisi tietoa suorituksen aikana. Tämä voi auttaa sinua löytämään ja korjaamaan virheitä nopeammin ja helpommin.