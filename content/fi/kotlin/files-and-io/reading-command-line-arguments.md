---
title:                "Komennoriviparametrien lukeminen"
aliases: - /fi/kotlin/reading-command-line-arguments.md
date:                  2024-01-20T17:56:17.180147-07:00
model:                 gpt-4-1106-preview
simple_title:         "Komennoriviparametrien lukeminen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
Komennon riviargumentit ovat komentoriviltä ohjelmiin syötettäviä tiedonpaloja. Niitä käytetään, koska ne mahdollistavat käyttäjän tarpeisiin mukautuvat ohjelmat, antavat vaihtoehtoja ja ohjaavat ohjelman suoritusta.

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
