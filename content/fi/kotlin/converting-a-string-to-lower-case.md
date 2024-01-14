---
title:    "Kotlin: Merkkijonon muuntaminen pieniksi kirjaimiksi"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi muuttaa merkkijono pieniksi kirjaimiksi?

Merkkijonon muuttaminen pieniksi kirjaimiksi voi olla hyödyllistä monissa ohjelmoinnin tilanteissa. Se voi helpottaa vertailua, kun halutaan tarkistaa, vastaako käyttäjän syöttämä sana ennalta määriteltyä sanaa. Se myös auttaa välttämään virheitä, jos esimerkiksi käytettävissä olevat tiedostot ovat eri kirjoitusmuodossa. Pienet kirjaimet ovat myös yleisesti hyväksytty tapa kirjoittaa ohjelmointikielistä.

## Kuinka muuttaa merkkijono pieniksi kirjaimiksi Kotlinissa

Kotlinissa merkkijonon pieniksi kirjaimiksi muuttaminen on helppoa. Tämä voidaan tehdä käyttämällä `toLowerCase()`-funktiota, joka kuuluu `String`-luokkaan. Alla on esimerkki kuinka käyttää tätä funktiota:

```Kotlin
val sana = "Tervetuloa"
val pienetKirjaimet = sana.toLowerCase()

println(pienetKirjaimet) // tulostaa "tervetuloa"
```

Tässä esimerkissä luodaan uusi merkkijono, joka sisältää sanan "Tervetuloa". Tämän jälkeen käytetään `toLowerCase()`-funktiota, joka muuttaa sanan pieniksi kirjaimiksi ja tallentaa sen uuteen muuttujaan. Lopuksi käytetään `println()`-funktiota tulostamaan uusi merkkijono konsoliin.

## Syvemmälle merkkijonon muuttamiseen pieniksi kirjaimiksi

Vaikka merkkijonon muuttaminen pieniksi kirjaimiksi Kotlinissa onkin helppoa, on hyvä ymmärtää miten tämä tapahtuu taustalla. `toLowerCase()`-funktio käyttää Unicode-standardia muuttaessaan merkkijonoa pieniksi kirjaimiksi. Unicode-standardissa jokaisella kirjaimella on oma koodipiste, joka määrittää sen suuren tai pienen kirjaimen. Kun funktio muuttaa merkkijonon pieniksi kirjaimiksi, se tarkistaa jokaisen kirjaimen koodipisteen ja muuttaa sen vastaavaksi pieneksi kirjaimeksi.

## Katso myös

- [Kotlin Standard Library](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-lower-case.html)
- [Unicode-standardi](https://unicode.org/standard/standard.html)