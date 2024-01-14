---
title:    "Kotlin: Komentoriviparametrien lukeminen"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Miksi

Command line argumenttien lukeminen on hyödyllinen taito jokaiselle Kotlin-kehittäjälle. Se mahdollistaa käyttäjän syöttämien tietojen hyödyntämisen ohjelmassa, mikä voi parantaa sen käytettävyyttä ja tehokkuutta.

## Kuinka

Command line argumenttien lukeminen Kotlinissa on helppoa. Se tehdään `main`-funktion parametrin `args` avulla. Alla on esimerkki:

```Kotlin
fun main(args: Array<String>) {
    for(argument in args) {
        println(argument)
    }
}

```

Tämän esimerkin avulla kaikki komentoriville syötetyt argumentit tulostetaan näytölle.

Esimerkiksi, jos annetaan seuraava komento:

`kotlin MyProgram argumentti1 argumentti2`

Ohjelman tulostus näyttäisi tältä:

`argumentti1 argumentti2`

## Deep Dive

Vaikka esimerkissämme käyttäjien syöttämät komentoriviparametrit tulostetaan vain näytölle, niitä voidaan käyttää monin eri tavoin ohjelmassa. Esimerkiksi, jos ohjelma tarvitsee tietyn tiedoston nimen, sen voi antaa komentorivillä parametrina ja lukea ohjelmassa `args`-muuttujasta.

Argumenttien lukemisessa tulee kuitenkin olla varovainen, sillä ne voivat aiheuttaa erilaisia ongelmia kuten virheitä ja tietoturvariskejä. On tärkeää tarkistaa, että käyttäjälle syötetyt argumentit ovat oikeassa muodossa ja että ne eivät aiheuta haittaa ohjelmalle.

## Katso myös

- [Official Kotlin Documentation on Command Line Arguments](https://kotlinlang.org/docs/tutorials/command-line.html#argument-parsing)
- [Blogikirjoitus "Reading Command Line Arguments in Kotlin"](https://www.baeldung.com/kotlin/command-line-arguments)
- [Stack Overflow viestiketju aiheesta "How to Read Command Line Arguments in Kotlin"](https://stackoverflow.com/questions/39855581/how-to-read-command-line-arguments-in-kotlin)