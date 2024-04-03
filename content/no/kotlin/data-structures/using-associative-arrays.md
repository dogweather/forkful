---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:48.150523-07:00
description: "Hvordan: \xC5 lage og bruke et kart i Kotlin er enkelt. Her er en rask\
  \ guide p\xE5 hvordan du gj\xF8r det."
lastmod: '2024-03-13T22:44:40.743049-06:00'
model: gpt-4-0125-preview
summary: "\xC5 lage og bruke et kart i Kotlin er enkelt."
title: Bruke associative tabeller
weight: 15
---

## Hvordan:
Å lage og bruke et kart i Kotlin er enkelt. Her er en rask guide på hvordan du gjør det:

```Kotlin
fun main() {
    // Oppretter et muterbart kart
    val fruits = mutableMapOf("a" to "Apple", "b" to "Banana")

    // Legger til elementer
    fruits["o"] = "Orange" // Bruker indekseringsoperasjon
    fruits.put("g", "Grape") // Bruker put-metoden

    // Får tilgang til elementer
    println(fruits["a"])  // Utdata: Apple
    println(fruits["b"])  // Utdata: Banana

    // Fjerner elementer
    fruits.remove("b")
    
    // Itererer over kartet
    for ((key, value) in fruits) {
        println("$key -> $value")
    }
    // Eksempel på utdata:
    // a -> Apple
    // o -> Orange
    // g -> Grape
}
```

## Dypdykk
Kotlin sine kart kommer direkte fra dens interoperabilitet med Java, der kart er en essensiell del av samlinger. Imidlertid forbedrer Kotlin deres brukervennlighet ved å tilby både mutable (`MutableMap`) og skrivebeskyttede (`Map`) grensesnitt, i motsetning til Javas forente `Map` grensesnitt. Denne distinksjonen gjør det klart om en samling er ment for modifikasjon eller ikke.

En betydelig detalj om Koltins kartimplementasjon er den eksplisitte forskjellen mellom mutable og umutable kart, som understreker språkets fokus på immutabilitet og trådsikkerhet.

Selv om kart er svært nyttige, tilbyr Kotlin også andre samlinger som lister og sett, hver med sitt eget bruksområde. For eksempel opprettholder lister rekkefølge og tillater duplikater, noe som gjør dem ideelle for å få tilgang til elementer ved indeks, mens sett sikrer unikhet, men ikke opprettholder rekkefølge. Valget mellom å bruke et kart, liste eller sett avhenger av de spesifikke kravene til applikasjonen din, slik som behovet for nøkkelbasert tilgang eller bevaring av orden.

Når det gjelder bedre alternativer, hvis ytelse er avgjørende, spesielt med store samlinger, vurder å bruke spesialiserte, mer effektive datastrukturer som tilbys av eksterne biblioteker som er optimalisert for spesifikke brukstilfeller, som samtidig tilgang eller sortering.
