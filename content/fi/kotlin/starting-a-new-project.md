---
title:                "Kotlin: Aloittamassa uutta projektia."
simple_title:         "Aloittamassa uutta projektia."
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Miksi

Miksi ihmiset haluavat aloittaa uuden ohjelmointiprojektin? Usein se johtuu siitä, että heillä on idea, jota haluavat toteuttaa tai he haluavat oppia uutta ohjelmointikieltä tai tekniikkaa. On myös mahdollista, että he haluavat luoda jotain hyödyllistä itselleen tai toisilleen.

## Kuinka

Kun aloitat uuden Kotlin-projektin, ensimmäinen askel on asentaa Kotlin-ympäristö ja luoda projekti kansioon, johon haluat tallentaa koodisi. Voit tehdä tämän käyttämällä seuraavia komentoja:

```Kotlin
sudo apt-get update
sudo apt-get install kotlin
mkdir project-folder
cd project-folder
```

Seuraavaksi voit aloittaa koodin kirjoittamisen. Tämä on yksinkertainen ohjelma, joka tulostaa "Hei maailma!" komentoriville:

```Kotlin
fun main() {
    println("Hei maailma!")
}
```

Voit ajaa tämän koodin käyttämällä seuraavaa komentoa:

```Kotlin
kotlinc program.kt -include-runtime -d program.jar
java -jar program.jar
```

Tämän pitäisi tulostaa "Hei maailma!" komentoriville.

## Syventävä sukellus

Kun aloitat uuden Kotlin-projektin, on tärkeää harkita, mitä koodeja tarvitset ja kuinka järjestät projektisi. Ensinnäkin, sinun on luotava `build.gradle`-tiedosto, johon voit lisätä tarvittavat riippuvuudet ja asetukset. Sinun pitäisi myös harkita projektisi rakennetta, jotta koodisi olisi helpompi ylläpitää ja laajentaa.

Kotlinin hyvä puoli on, että se on yhteensopiva Java-koodin kanssa, mikä tarkoittaa, että voit käyttää Java-kirjastoja ja työkaluja projekteissasi. Voit myös käyttää Kotlinin sisäistä työkalua, `kotlinc`, joka antaa sinun kääntää koodisi ilman, että tarvitset muita työkaluja tai kirjastoja.

## Katso myös

Suosittelemme seuraavia resursseja, jos haluat oppia lisää Kotlin-ohjelmoinnista:

- [Kotlinin virallinen verkkosivusto](https://kotlinlang.org/)
- [JetBrainsin Kotlin Bootcamp -kurssi](https://www.jetbrains.com/help/education/kotlin-bootcamp.html)
- [Kotlinin Slack-yhteisö](https://kotlinlang.slack.com/)