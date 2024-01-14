---
title:    "Kotlin: Lukeminen komentoriviparametreista"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi lukea komentorivin argumentit?

Komentorivin argumentit ovat tärkeä osa jokapäiväistä ohjelmoinnin maailmaa. Ne antavat ohjelman suorittajalle mahdollisuuden antaa lisäohjeita suoritettavalle ohjelmalle ilman, että koodiä joutuu muokkaamaan. Tämä tarkoittaa, että ohjelman käyttäjä voi muokata ohjelman toimintaa ilman tarvetta ohjelmointitaitoihin.

## Miten lukea komentorivin argumentit?

Kotlinilla on helppo lukea ja käsitellä komentorivin argumentteja. Käytämme tähän tarkoitukseen `args` muuttujaa, joka sisältää listan kaikista komentorivin argumenteista.

```Kotlin
fun main(args: Array<String>) {
    if (args.isNotEmpty()) {
        for (arg in args) {
            println(arg)
        }
    } else {
        println("No arguments passed!")
    }
}
```

Jos esimerkiksi suorittaisimme tämän ohjelman komentoriviltä seuraavasti: `kotlinc main.kt && kotlin Main abc def`, saisimme seuraavan tulosteen:

```
abc
def
```

Kun olemme saaneet `args` listan, voimme käsitellä ja käyttää argumentteja haluamallamme tavalla.

## Syvempi sukellus komentorivin argumenttien lukemiseen

Komentorivin argumentteja voidaan lukea myös järjestelmän ympäristömuuttujista, mikä tekee niistä entistä monipuolisempia. Näitä muuttujia voidaan muokata esimerkiksi `.bashrc` tai `.bash_profile` tiedoston kautta, jolloin ohjelmoija voi antaa oletusarvoja komentorivin argumenteille. Tämä tekee ohjelmasta joustavamman ja helpommin räätälöitävän käyttäjälle.

## Katso myös

- [Kotlinin virallinen dokumentaatio komentorivin argumenteista](https://kotlinlang.org/docs/reference/compiler-plugins.html)
- [Kuinka luoda komentorivipohjainen ohjelma Kotlinilla](https://www.raywenderlich.com/7258573-command-line-programs-on-the-jvm-with-kotlin)
- [Kotlinin ohjeet hakasulkujen käytöstä kertakäyttöisten komentorivin argumenttien kanssa](https://kotlinlang.org/docs/reference/compiler-plugin-reference.html#target-jvmargs)