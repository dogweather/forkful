---
title:    "C#: Jonojen yhdistäminen"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Miksi

Ohjelmoinnissa on useita tilanteita, joissa haluamme yhdistää merkkijonoja yhdeksi kokonaisuudeksi. Tätä kutsutaan merkkijonojen konkatenoimiseksi ja se on tarpeellinen työkalu monissa sovelluksissa. Joten miksi meidän pitäisi käyttää tätä toimintoa? 

Merkkijonokonkatenaatiolla pystymme luomaan dynaamisia tekstipohjaisia sovelluksia, kuten lomakkeita, käyttäjäystävällisiä käyttöliittymäelementtejä ja paljon muuta. Kun yhdistämme erillisiä merkkijonoja, voimme luoda räätälöityjä tulosteita käyttäen muuttujia ja ehtolauseita. Se myös auttaa tekstin muotoilun hallinnassa, sillä haluamme usein lisätä välejä tai muita erikoismerkkejä merkkijonoihin. Nämä ovat vain muutamia syitä, miksi merkkijonokonkatenaatio on tärkeää ohjelmoinnissa.

## Miten se tehdään: 

```C#
// Yhdistetään kolme merkkijonoa
string esimerkkiMerkkijono1 = "Tervetuloa ";
string esimerkkiMerkkijono2 = "ohjelmointiblogiin! ";
string esimerkkiMerkkijono3 = "Täällä opit kaiken tarvittavan C# -kielestä.";

// Käytetään konkatenaatiota yhdistämään merkkijonot yhdeksi
string tulos = esimerkkiMerkkijono1 + esimerkkiMerkkijono2 + esimerkkiMerkkijono3;

// Tulostetaan tulos tekstinä
Console.WriteLine(tulos);

// Tuloste: Tervetuloa ohjelmointiblogiin! Täällä opit kaiken tarvittavan C# -kielestä.
```
 
Kuten näemme esimerkissä, käytämme plus -merkkiä (+) yhdistämään merkkijonoja toisiinsa. Voimme myös käyttää muuttujia ja muita arvoja konkatenaation aikana, mikä tekee siitä erittäin monipuolisen työkalun. Muista myös lisätä välilyöntejä ja muita merkkejä tarvittaessa.

Voit myös käyttää `String.Concat()` -funktiota yhdistämään useampia merkkijonoja kerrallaan. Esimerkiksi:

```C#
// Yhdistetään neljä merkkijonoa
string osa1 = "Tämä ";
string osa2 = "on ";
string osa3 = "koko ";
string osa4 = "lause.";

// Käytetään Concat() -funktiota yhdistämään merkkijonot
string tulos = String.Concat(osa1, osa2, osa3, osa4);

// Tulostetaan tulos tekstinä
Console.WriteLine(tulos);

// Tuloste: Tämä on koko lause.
```

## Syventyvä tarkastelu

Merkkijonokonkatenaatio on erittäin tärkeä osa ohjelmoinnin perusteita ja se löytyy useimmista ohjelmointikielistä. C# -kielellä meillä on kuitenkin muitakin vaihtoehtoja konkatenaation lisäksi. Voimme esimerkiksi käyttää `StringBuilder`-luokkaa, joka tarjoaa tehokkaamman tavan yhdistää useita merkkijonoja. Tämä on hyödyllistä, jos meidän täytyy yhdistää paljon merkkijonoja, sillä se vähentää muistin käyttöä ja nopeuttaa koodin suoritusta. 

Toinen asia, jota kann