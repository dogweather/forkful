---
title:                "Kotlin: Lukeminen komentoriviparametreista"
simple_title:         "Lukeminen komentoriviparametreista"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi
Yksi tärkeimmistä taidoista modernissa ohjelmoinnissa on kyky lukea komentorivin argumentteja. Se säästää aikaa ja vaivaa manuaalisen syöttämisen sijaan. Lisäksi, se antaa mahdollisuuden tehdä ohjelmistosta dynaamisemman ja monipuolisemman käyttäjille.

## Kuinka
Kotlin tarjoaa helpon tavan lukea komentorivin argumentteja käyttämällä `args` muuttujaa. Tämän muuttujan avulla voit päästä käsiksi kaikkiin argumentteihin, jotka on syötetty ohjelman suorituksen alussa. Alla on yksinkertainen esimerkki:

```Kotlin
fun main(args: Array<String>){ 
    println("Komentorivin argumentit:") 
    for (arg in args) { 
        println("$arg") 
    } 
}
```

Jos tämä koodi ajetaan komentorivillä käyttäen esimerkiksi `kotlin CommandLine.kt arg1 arg2 arg3`, nähdään seuraava tulos:

```
Komentorivin argumentit:
arg1
arg2
arg3
```

Kuten näet, kaikki argumentit on tallennettu `args` muuttujaan ja niitä voidaan käsitellä haluamallasi tavalla. Voit myös luoda ehtolauseita, jotka tarkistavat, ovatko tietyt argumentit mukana ja suorittavat sen perusteella erilaisia toimintoja.

## Syvällinen tarkastelu
Käyttämällä `args` muuttujaa, et ole rajoitettu vain lukemaan tekstipohjaisia argumentteja. Voit myös lukea numeerisia argumentteja ja jopa tiedostoja ja kansioita. Alla on esimerkki, jossa luetaan neljä numeerista argumenttia, lasketaan niiden summa ja tulostetaan se:

```Kotlin
fun main(args: Array<String>) {
  if(args.size < 4){ 
        println("Tarvitset neljä argumenttia!") 
        return 
    } 

    var sum = 0 

    for(i in 1..4){ 
        sum += args[i-1].toInt() 
    } 

    println("Summa on: $sum") 
}
```

Jos ajetaan esimerkiksi `kotlin ArgumenttienLaskeminen.kt 1 2 3 4`, nähdään seuraava tulos:

```
Summa on: 10
```

Kuten näet, voit käyttää `args` muuttujaa käsittelemään erilaisia argumentteja ja suorittamaan monimutkaisempia tehtäviä ohjelmassa. Se on hyödyllinen taito, jota kannattaa opetella.

## Katso myös
- [Kotlinin `args` muuttujan dokumentaatio](https://kotlinlang.org/docs/command-line.html#passing-command-line-arguments-to-the-main-function)
- [Kotlinin viralliset verkkosivut](https://kotlinlang.org/)
- [Ohjelmointiopetusvideot suomeksi](https://fi.wikipedia.org/wiki/Luokka:Ohjelmointiopetusvideot)