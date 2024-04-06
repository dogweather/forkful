---
date: 2024-01-20 17:56:54.670264-07:00
description: "How to: (Miten:) Swiftiss\xE4 komentorivin argumentit luetaan `CommandLine`-luokan\
  \ avulla. T\xE4ss\xE4 yksinkertainen esimerkki."
lastmod: '2024-04-05T21:53:58.501136-06:00'
model: gpt-4-1106-preview
summary: "(Miten:) Swiftiss\xE4 komentorivin argumentit luetaan `CommandLine`-luokan\
  \ avulla."
title: Komennoriviparametrien lukeminen
weight: 23
---

## How to: (Miten:)
Swiftissä komentorivin argumentit luetaan `CommandLine`-luokan avulla. Tässä yksinkertainen esimerkki:

```Swift
// Pääohjelma

// Tulosta kaikki komentorivin argumentit
for argument in CommandLine.arguments {
    print(argument)
}

// Käsittele argumentit
if CommandLine.arguments.count > 1 {
    let argument = CommandLine.arguments[1]
    print("Ensimmäinen argumentti on: \(argument)")
} else {
    print("Argumentteja ei annettu.")
}
```

Kun suoritat yllä olevan ohjelman, saat esimerkiksi seuraavaa tulostetta:
```
$ swift run MyProgram param1 param2
./MyProgram
param1
param2
Ensimmäinen argumentti on: param1
```

## Deep Dive (Sukellus syvemmälle)
Komentorivin argumentit ovat peruja UNIX-järjestelmien ajoilta ja ovat edelleen pätevä tapa antaa ohjelmalle tietoja. Swiftissä `CommandLine` on perinteinen tapa argumenttien käsittelyyn; alternatiivina voidaan käyttää esimerkiksi Apple'n ArgumentParser-kirjastoa, joka tarjoaa kehittyneempää argumenttien käsittelyä. Käytännössä, `CommandLine`-luokka tallentaa argumentit `arguments`-taulukkoon, jonka indeksi alkaa nollasta (ohjelman polku) ja jatkuu annettujen argumenttien mukaisesti.

## See Also (Katso myös)
- Swift.org:n dokumentaatio komentorivin argumenteista: [Swift.org Documentation](https://www.swift.org)
- GitHub-repo Swift Argument Parserille: [Swift Argument Parser GitHub](https://github.com/apple/swift-argument-parser)
