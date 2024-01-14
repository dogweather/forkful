---
title:                "Kotlin: Työskentely yaml:n kanssa"
simple_title:         "Työskentely yaml:n kanssa"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/working-with-yaml.md"
---

{{< edit_this_page >}}

## Miksi YAML on hyödyllinen ohjelmoinnissa?

YAML (Yet Another Markup Language) on rakenteellinen tiedon esittämiseen tarkoitettu formaatti, joka on yhä enenevässä määrin suosittu ohjelmointiyhteisössä. Sen helppolukuisuus ja yksinkertaisuus tekevät siitä erinomaisen vaihtoehdon ohjelmointikielten ja ohjelmien konfiguraatiolle. 

## Kuinka käyttää YAML-formaattia Kotlinissa?

Kotlin on moderni ohjelmointikieli, joka tarjoaa käyttäjilleen monipuolisen kirjaston erilaisia toimintoja varten. Tässä osassa näytämme, kuinka luoda YAML-tiedosto Kotlin-ohjelmassa ja käyttää sitä eri tavoin. 

### Luominen ja muokkaaminen

Kotlinissa YAML-tiedoston luominen on helppoa. Käytämme siihen YAML-kirjastoa, ja aloitamme kertomalla sille minkä tiedostonimen haluamme luoda ja mihin hakemistoon sen tallennamme. Tämän jälkeen voimme lisätä haluamamme tiedot ja rakenteet käyttämällä YAML-kirjaston metodeja. Alla on esimerkki YAML-tiedoston luomisesta ja muokkaamisesta Kotlinissa:

```Kotlin
// Importoi YAML-kirjasto
import org.yaml.snakeyaml.Yaml

// Määritä tiedostonimi ja tallennushakemisto
val tiedostoNimi = "esimerkkitiedosto.yaml"
val hakemisto = "resurssit"

// Luodaan YAML-tiedosto ja tallennetaan siihen tietoja
val yaml = Yaml()
val tietoja = HashMap<String, Any>()
tietoja["nimi"] = "John Doe"
tietoja["ikä"] = 30
tietoja["työpaikka"] = "Ohjelmoija"

// Kirjoitetaan tietoja YAML-tiedostoon
FileOutputStream(tiedostoNimi).use { tiedosto ->
  // YAML-kirjaston metodi kirjoittaa tiedot tiedostoon
  yaml.dump(tietoja, tiedosto)
}
```

Koodin suorituksen jälkeen tiedostoon tallentuu seuraavat tiedot:

```Kotlin
nimi: John Doe
ikä: 30
työpaikka: Ohjelmoija
```

### Lukeminen ja käyttäminen

Luotu YAML-tiedosto voidaan lukea ja käyttää helposti eri ohjelmissa ja sovelluksissa. Käytämme tätä varten YAML-kirjastoa ja sen metodeja. Alla on esimerkki YAML-tiedoston lukemisesta ja sen sisällön käyttämisestä Kotlinissa:

```Kotlin
// Avataan YAML-tiedosto ja luetaan sen sisältö
val sisältö = File(tiedostoNimi).readText()
val lukija = Yaml().loadAs(sisältö, HashMap::class.java)

// Käydään läpi sisältö ja tulostetaan se konsoliin
for ((avain, arvo) in lukija) {
  println("$avain: $arvo")
}
```

Tämän koodin suorituksen jälkeen tulostuu konsoliin seuraavat tiedot:

```Kotlin
nimi: John Doe
ikä: 30
työpaikka: Ohjelmoija
```

## Syvempi sukellus YAML:in

YAML tarjoaa käyttäjilleen monia erilaisia mahdollisuuksia rakentaa jäsenneltyjä tiedostoja ja tietorakenteita. Sen avulla voidaan luoda esimerkiksi konfiguraatiotiedostoja ja luettelotietoja eri muodoissa. Tutustutaan seuraavaksi muutamaan YAML:n käyttöön liittyvään käsitteeseen. 

### Luokat ja olio