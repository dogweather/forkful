---
title:                "Luku komentoriviparametreista"
html_title:           "Swift: Luku komentoriviparametreista"
simple_title:         "Luku komentoriviparametreista"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Komentorivin argumenttien lukeminen on yksinkertaisesti prosessi, jossa ohjelma lukee ja käsittelee käyttäjän syöttämiä arvoja komentorivillä. Ohjelmoijat käyttävät tätä prosessia esimerkiksi ohjelman eri toiminnallisuuksien valitsemiseen tai datan syöttämiseen ohjelmaan.

## Miten:
```Swift
let argumentit = CommandLine.arguments

// Tulostaa kaikki komentoriviltä annetut argumentit
for argumentti in argumentit {
    print(argumentti)
}

```

Esimerkkitulostus: 
```
OhjelmanNimi argumentti1 argumentti2

argumentti1
argumentti2
```

## Syvällinen sukellus:
Komentorivin argumenttien lukeminen on ollut osa ohjelmointia jo pitkään ja se on yksinkertainen tapa lukea käyttäjän syöttämiä arvoja ohjelmaan. Mikäli haluaa tutustua muihin tapoihin lukea syötteitä käyttäjältä, kannattaa tutustua esimerkiksi lukija-olioon (Scanner) tai käyttäjän syötteen lukemiseen standardi inputilta. Komentorivin argumenttien lukemisen toteutus perustuu CMDLine luokkaan ja sen metodeihin.

## Katso myös:
- [Swiftin dokumentaatio komentorivin argumenttien lukemisesta](https://developer.apple.com/documentation/swift/commandline)
- [Lukija-olion (Scanner) käyttö Swiftissä](https://www.hackingwithswift.com/example-code/language/how-to-read-from-the-command-line-using-scanner)