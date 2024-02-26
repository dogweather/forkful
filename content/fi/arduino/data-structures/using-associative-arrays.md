---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:26.217679-07:00
description: "Arduinon maailmassa assosiatiiviset taulukot mahdollistavat avainten\
  \ yhdist\xE4misen arvoihin, melkein kuin yhdist\xE4isit sukkia pareiksi. Ne ovat\
  \ hy\xF6dyllisi\xE4,\u2026"
lastmod: '2024-02-25T18:49:53.727927-07:00'
model: gpt-4-0125-preview
summary: "Arduinon maailmassa assosiatiiviset taulukot mahdollistavat avainten yhdist\xE4\
  misen arvoihin, melkein kuin yhdist\xE4isit sukkia pareiksi. Ne ovat hy\xF6dyllisi\xE4\
  ,\u2026"
title: "Assosiatiivisten taulukoiden k\xE4ytt\xF6"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Arduinon maailmassa assosiatiiviset taulukot mahdollistavat avainten yhdistämisen arvoihin, melkein kuin yhdistäisit sukkia pareiksi. Ne ovat hyödyllisiä, kun tarvitset tallentaa ja hakea tietoa käyttäen kuvaavia nimiä, tehden koodistasi siistimpää ja paljon ymmärrettävämpää.

## Kuinka:
Arduino, tiukasti ottaen, ei tue assosiatiivisia taulukoita samalla tavalla kuin korkeamman tason kielissä. Mutta, älä pelkää. Voimme olla nokkelia käyttämällä rakenteita ja taulukoita jäljitelläksemme tätä toiminnallisuutta. Tässä on yksinkertainen esimerkki perustason "assosiatiivisen taulukon" luomisesta eri kaupunkien lämpötilojen tallentamiseen ja hakemiseen.

Ensiksi, määrittele rakenne kaupungin (avain) ja sen lämpötilan (arvo) säilyttämiseen:

```cpp
struct CityTemperature {
  String city;
  float temperature;
};
```

Seuraavaksi, alusta `CityTemperature`-objektien taulukko:

```cpp
CityTemperature temperatures[] = {
  {"New York", 19.5},
  {"Los Angeles", 22.0},
  {"Chicago", 17.0}
};
```

Näin voit käyttää ja näyttää tietyn kaupungin lämpötilan:

```cpp
void setup() {
  Serial.begin(9600);
  for(int i = 0; i < 3; i++) {
    if(temperatures[i].city == "Los Angeles") {
      Serial.print("Lämpötila Los Angelesissa on: ");
      Serial.println(temperatures[i].temperature);
    }
  }
}

void loop() {
  // Täällä ei ole mitään toistaiseksi.
}
```

Tämän koodin suorittaminen antaisi tulosteeksi:

```
Lämpötila Los Angelesissa on: 22.0
```

## Syväsukellus
Historiallisesti ohjelmointikielet kuten C ja C++ (joista Arduinon syntaksi on johdettu) eivät tulleet sisäänrakennettujen assosiatiivisten taulukoiden kanssa, mikä johti yllä näytettyihin kiertotapoihin. Tämä lähestymistapa on suhteellisen yksinkertainen, mutta skaalautuu huonosti, kun datan koko kasvaa johtuen sen O(n) hakemisajasta.

Kielet kuten Python tarjoavat sanakirjoja ja JavaScriptillä on objektit tähän tarkoitukseen, jotka molemmat ovat paljon tehokkaampia avain-arvo parien hallinnassa. Arduinossa, kun suorituskyky ja tehokkuus muodostuvat kriittisiksi, kehittäjät voivat valita erikoistuneempia tietorakenteita, kuten hajautustauluja, jotka toteutetaan kirjastojen avulla.

Vaikka Arduino ei alkuperäisesti tue assosiatiivisia taulukoita, yhteisö on kehittänyt kirjastoja kuten `HashMap`, jotka voidaan lisätä projektiisi tarjoten samankaltaista toiminnallisuutta paremmalla suorituskyvyllä kuin DIY-lähestymistavalla. Nämä kirjastot tarjoavat tyypillisesti tyylikkäämmän ja tehokkaamman tavan hallita assosiatiivisia taulukoita, erityisesti monimutkaisemmissa projekteissa.
