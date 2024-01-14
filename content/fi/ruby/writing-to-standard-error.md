---
title:    "Ruby: Kirjoittaminen standardivirheelle"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Miksi

Jokainen ohjelmoija tuntee tarpeen kirjoittaa koodiinsa virheitä varten. Tämä auttaa meitä havaitsemaan ja korjaamaan ongelmia ohjelman suorituksen aikana. Yksi tapa tehdä tämä on kirjoittaa virheet standardi virhepuskuriin, jota kutsutaan myös nimillä "STDERR" tai "standard error". Tässä blogikirjoituksessa opimme, miksi ja miten kirjoittaa standardi virhepuskuriin Ruby-koodissa.

## Miten

Kaikki Ruby-skriptit muodostuvat yhdestä tai useammasta koodista, jotka suoritetaan peräkkäin. Tämä koodi löytyy useinkaanut “main” -muuttujasta. Voit käyttää "puts" -metodia tulostamaan mitä tahansa haluat, mutta miten tulostetaan virheitä standardi virhepuskuriin? Tätä varten käytämme "STDERR.puts" -metodia. Katso alla olevaa esimerkkiä:

````Ruby
# Luo virhe ilmoitus
virheilmoitus = "Virhe: Tallennettavaa tietoa ei löytynyt"

# Tulosta virhe standardi virhepuskuriin
STDERR.puts virheilmoitus
````

Tämä koodi tuottaa seuraavan tulosteen:

````bash
# Virhe: Tallennettavaa tietoa ei löytynyt
````

Hienoa, olemme nyt onnistuneesti kirjoittaneet virheen standardi virhepuskuriin!

## Syventävä katsaus

Virheiden kirjoittaminen standardi virhepuskuriin on hyödyllistä, koska se auttaa meitä erottamaan normaalit tulosteet ja ilmoittamaan virheistä suoraan ohjelman suorituksen aikana. Lisäksi voimme käyttää "STDERR.puts" -metodia myös tarkistaakseen, kuinka pitkälle ohjelma on edennyt, joskus se saattaa lopettaa suorituksen jonkin vaiheen jälkeen. Näin voimme selvittää, mikä vaihe aiheutti ongelman ja korjata sen helpommin.

On myös hyvä huomata, että Ruby-kielellä on muitakin vaihtoehtoja virheiden käsittelyyn, kuten "raise" -lauseke, joka sallii meidän heittää virheen tai poikkeuksen haluamaamme kohtaan ohjelmassa.

## Katso myös

- [Virheiden käsittely Ruby-kielessä](https://rubyonrails.org/core/classes/Exception.html)
- [Ruby-dokumentaatio: STDERR](https://ruby-doc.org/core-2.6.3/STDERR.html)
- [Ruby-dokumentaatio: raise-lauseke](https://ruby-doc.org/core-2.6.3/Kernel.html#method-i-raise)