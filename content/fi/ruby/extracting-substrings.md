---
title:    "Ruby: Alistringien erottaminen"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Miksi

Saatat ihmetellä, miksi haluaisit ikinä käyttää aikaa etsien ja erottamalla merkkijonoja koodauksen aikana. Kuitenkin, jos olet rakentamassa sovellusta, joka käsittelee käyttäjän syötteitä tai käsittelee tekstejä, substringsien erotteleminen on välttämätöntä. Substringsit ovat tietyn mittaisia merkkijonoja, jotka on erotettu isommasta merkkijonosta. Tämä taito on hyödyllinen monessa eri käyttötapauksessa ja on ehdottomasti kannattava opetella.

## Miten tehdä

Jotta voit erotella substringsit Rubyssa, voit käyttää hyväksesi String-luokan eri metodeja. Esimerkiksi, voit käyttää "slice" -metodia, joka palauttaa halutun osan merkkijonosta. Käytännön esimerkki näyttää tältä:

```Ruby
# Luo merkkijono
sana = "Meri on sininen."

# Erottele merkkijonon ensimmäinen osa
sana.slice(0,4) #=> "Meri"

# Erottele merkkijonon toinen osa
sana.slice(5,2) #=> "on"
```

Kuten näet, "slice" -metodi hyväksyy kaksi argumenttia: ensimmäisenä substringin aloituskohdan ja toisena substringin pituuden. Voit myös käyttää muita metodeja, kuten "sub" ja "gsub", etsimään ja korvaamaan merkkijonosta tietyt osat.

## Syväsukellus

Voit myös käyttää säännöllisiä lausekkeita (regex) erottamaan substringsit merkkijonosta. Tämä on erittäin hyödyllistä, jos haluat etsiä tiettyjä kuvioita merkkijonosta. Esimerkiksi, voit käyttää säännöllistä lauseketta etsimään kaikki numerot merkkijonosta.

```Ruby
# Luo merkkijono
lause = "Tänään on 17. huhtikuuta."

# Etsi ja erottele numerot
lause.scan(/\d+/) #=> ["17"]
```

Säännölliset lausekkeet voivat tuntua aluksi haastavilta, mutta ne ovat erittäin hyödyllisiä ja tarpeellisia työkaluja koodarille. Suosittelen tutustumaan niihin tarkemmin ja kokeilemaan niiden käyttöä substringien erottelussa.

## Katso myös

Kuten huomaat, substringsien erottelu Rubyssa on suhteellisen yksinkertaista. Voit oppia lisää aiheesta tutkimalla String-luokan eri metodeja ja kokeilemalla niitä käytännössä. Tutustu myös säännöllisiin lausekkeisiin ja niiden käyttöön substringien erottelussa.

Lisäresursseja:

- [Ruby String-luokan dokumentaatio](https://ruby-doc.org/core-3.0.0/String.html)
- [Säännöllisten lausekkeiden opas Rubyssa](https://www.regular-expressions.info/)
- [Ruby Learning Center](https://ruby-doc.com/docs/ProgrammingRuby/)