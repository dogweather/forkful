---
title:    "C#: Merkkijonon muuttaminen pieniksi kirjaimiksi"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Miksi
Joskus ohjelmoinnin yhteydessä saattaa olla tarpeellista muuttaa merkkijono pieniksi kirjaimiksi. Tämä voi olla hyödyllistä esimerkiksi vertaillessa merkkijonoja tai tietokantaoperaatioita suoritettaessa. Tässä blogikirjoituksessa kerron, miten tämä voidaan tehdä C# -kielellä.

# Kuinka
```C#
string s = "TÄMÄ ON ESIMERKKI";
string lowerCase = s.ToLower();
Console.WriteLine(lowerCase);
```
Tämän yksinkertaisen koodiesimerkin avulla voit muuttaa merkkijonon pieniksi kirjaimiksi käyttämällä ToLower-metodia. Tässä esimerkissä tulostuu "tämä on esimerkki".

# Syväsukellus
ToLower-metodi käyttää kohdekielelle määritettyä kielimuunninta muuttaakseen merkkijonon alkuperäisenä olevan referenssin s ja palauttaa uuden merkkijonon pienellä kirjaimella. Tämän avulla varmistetaan, ettei alkuperäinen merkkijono muutu.

ToLower-metodin käyttö on myös nopeampaa kuin koko merkkijonon kirjain kerrallaan käymiseen tarvittava algoritmi. Tämä johtuu siitä, että käytettävä kielimuunnin on optimoitu tehtävänsä suorittamiseen ja voi hyödyntää kohdekielelle määritettyjä tietorakenteita.

# Katso myös
- [C#-kielen virallinen dokumentaatio](https://docs.microsoft.com/fi-fi/dotnet/api/system.string.tolower?view=netcore-3.1#System_String_ToLower)
- [Kielimuunnin - Wikipedia]()
- [Kuinka käyttää kielimuunninta tehokkaasti? - Stack Overflow](https://stackoverflow.com/questions/2658033/how-to-use-a-locale-specific-sorteddictionaryefficiently)

---
Jos sinulla on lisäkysymyksiä tai haluat jakaa kokemuksiasi, jätä kommentti alle! Kiitos lukemisesta. Nähdään seuraavassa blogikirjoituksessani!

# Katso myös