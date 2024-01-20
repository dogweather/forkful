---
title:                "Komentorivin argumenttien lukeminen"
html_title:           "Bash: Komentorivin argumenttien lukeminen"
simple_title:         "Komentorivin argumenttien lukeminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Komentoriviparametrien lukeminen tarkoittaa käyttäjän syöttämien parametrien käsittelyä ohjelmaa suoritettaessa. Programmeeraajien on tehtävä tämä voidakseen muokata ohjelman käyttäytymistä lennossa.

## Miten tehdä:

Kotlinissa komentoriviparametrien lukeminen on suoraviivaista. Käynnistysparametrit sisältyvät `main`-funktion argumentteihin, jotka ovat String-taulukon muodossa. Tarkastele esimerkkiä:

```Kotlin
fun main(args: Array<String>) {
   if(args.isNotEmpty()){
       println("Hei, ${args[0]}!")
   } else {
       println("Hei, tuntematon!")
   }
}
```

Jos tämän ohjelman suorittaa komennolla `kotlin HelloWorld.kt Mikko`, ohjelma tulostaa `Hei, Mikko!`. Jos ohjelman suorittaa ilman argumentteja, tulostuu `Hei, tuntematon!`.

## Syvällisempi sukellus:

Historiallisesti komentoriviparametreja on käytetty antamaan ohjelmalle tietoja sen suorituksen alussa. Vaikka nykyään on kehittyneempiä tapoja saada tietoja ohjelmaan (esimerkiksi käyttöliittymien tai verkkopalveluiden kautta), komentoriviparametreja käytetään silti yleisesti käsikirjoituksissa ja työkaluissa.

Myös muita tapoja komentoriviparametrien käsittelyyn on olemassa. Esimerkiksi `CommandLine`-kirjasto tarjoaa paljon toimintoja kuten lippuargumentteja ja argumenttien validointia.

Kotlinin `main`-funktiolle komentoriviparametrien syöttäminen perustuu Javalle tuttuun tyyliin. Käynnistysparametrit syötetään String-taulukkoon, joka toimitetaan `main`-funktolle.

## Katso myös:

- [Kotlinin viralliset dokumentit komentorivisovellusten luomisesta](https://kotlinlang.org/docs/command-line.html)
- [CommandLine-kirjasto](https://picocli.info/)