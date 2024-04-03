---
date: 2024-01-20 17:56:17.180147-07:00
description: "Komennon riviargumentit ovat komentorivilt\xE4 ohjelmiin sy\xF6tett\xE4\
  vi\xE4 tiedonpaloja. Niit\xE4 k\xE4ytet\xE4\xE4n, koska ne mahdollistavat k\xE4\
  ytt\xE4j\xE4n tarpeisiin mukautuvat\u2026"
lastmod: '2024-03-13T22:44:56.546799-06:00'
model: gpt-4-1106-preview
summary: "Komennon riviargumentit ovat komentorivilt\xE4 ohjelmiin sy\xF6tett\xE4\
  vi\xE4 tiedonpaloja."
title: Komennoriviparametrien lukeminen
weight: 23
---

## How to:
Kotlinissa komennon riviargumentit luetaan `Array<String>`-tyyppisenä `args`-parametrina pääfunktiolle. Tässä esimerkki:

```Kotlin
fun main(args: Array<String>) {
    if (args.isNotEmpty()) {
        println("Hei, ${args[0]}!")
    } else {
        println("Hei, tuntematon!")
    }
}
```

Komentoriviltä ajettaessa:

```
> kotlinc MyProgram.kt -include-runtime -d MyProgram.jar
> java -jar MyProgram.jar Tero
Hei, Tero!

> java -jar MyProgram.jar
Hei, tuntematon!
```

## Deep Dive
Komentoriviparametrien lukeminen on peruskauraa ohjelmoinnissa. Ne ovat olleet mukana ohjelmistokehityksen alkuaikoina ja ovat edelleen tärkeitä erityisesti skripteissä ja työkaluissa. Kotlinissa ne tulevat suoraan Java-perinnöstä.

Vaihtoehtoisesti voit käyttää kirjastoja, kuten `kotlinx.cli`, joka tarjoaa monipuolisemmat vaihtoehdot ja helpompaa hallintaa.

Tarkasti ottaen, `args` on muuttumaton taulukko (`Array`), joten sen kokoa ei voi muuttaa suorituksen aikana. Argumentit tulevat ohjelmaan sillä järjestyksellä, kun ne on kirjoitettu komentoriville.

## See Also
- Kotlinin virallinen dokumentaatio komentoriviparametreista: [CommandLine](https://kotlinlang.org/docs/faq.html#where-can-i-learn-about-command-line-arguments-in-kotlin)
- `kotlinx.cli` kirjaston GitHub-sivu: [kotlinx.cli](https://github.com/Kotlin/kotlinx-cli)
- Oracle Java-tutoriaali komentoriviparametreista: [Oracle Docs](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
