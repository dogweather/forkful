---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:30.795295-07:00
description: "Hvordan: Selv om Go ikke inkluderer en innebygd REPL, har samfunnet\
  \ skapt verkt\xF8y som `gore` for \xE5 fylle dette gapet. F\xF8rst, installer `gore`\
  \ ved \xE5 kj\xF8re."
lastmod: '2024-03-13T22:44:40.268656-06:00'
model: gpt-4-0125-preview
summary: "Selv om Go ikke inkluderer en innebygd REPL, har samfunnet skapt verkt\xF8\
  y som `gore` for \xE5 fylle dette gapet."
title: Bruk av interaktiv shell (REPL)
weight: 34
---

## Hvordan:
Selv om Go ikke inkluderer en innebygd REPL, har samfunnet skapt verktøy som `gore` for å fylle dette gapet. Først, installer `gore` ved å kjøre:

```
$ go get -u github.com/motemen/gore
```

Når installert, start `gore` ved å skrive `gore` i terminalen din:

```
$ gore
```

Du vil se en ledetekst klar til å akseptere Go-kommandoer. La oss prøve et enkelt eksempel:

```
gore> :import fmt
gore> fmt.Println("Hello, Go REPL!")
```

Du vil se utskrift som:

```
Hello, Go REPL!
```

Variabler og funksjonsdefinisjoner fungerer som forventet. Du kan erklære en funksjon:

```
gore> :import math
gore> areaCircle := func(radius float64) float64 {
...> return math.Pi * radius * radius
...> }
gore> fmt.Println("Området av en sirkel med radius 4:", areaCircle(4))
```

Og få utskriften med en gang:

```
Området av en sirkel med radius 4: 50.26548245743669
```

## Dypdykk:
Konseptet med en REPL er eldgammelt, som går tilbake til Lisp-maskinene på 1960-tallet, og tilbyr en interaktiv programmeringsopplevelse. I motsetning til språk som Python eller JavaScript, ble Go designet uten en REPL, med fokus i stedet på kompilerte binærfiler for ytelse og enkelhet. Dette reflekterer Gos filosofi om enkelhet og dets design for skalerbar og vedlikeholdbar programvare.

Verktøy som `gore` eller `goplay`, derimot, fremhever Go-samfunnets ressursstyrke i å bygge bro over dette gapet. Disse verktøyene parser Go-kode dynamisk og bruker `go/eval`-pakken eller lignende mekanismer til å kjøre den i sanntid, om enn med noen begrensninger sammenlignet med et opprinnelig REPL-miljø. Disse begrensningene stammer fra Gos typsystem og kompileringsmodell, som kan gjøre utførelse på stående fot utfordrende.

Selv om REPL-miljøer er ekstremt nyttige for utdanning og raske tester, har Go-økosystemet vanligvis en tendens til å helle mot tradisjonelle kompiler-og-kjør-prosesser for de fleste utviklingsoppgaver. IDEer og redaktører med støtte for Go, som Visual Studio Code eller GoLand, tilbyr integrerte verktøy for testing og feilsøking som i stor grad reduserer behovet for en REPL for profesjonell utvikling.

For utforskende programmering, prototyping eller læring, tilbyr imidlertid REPL-er som `gore` et verdifullt alternativ, som tillater programmerere vant til REPLs i andre språk å nyte en lignende opplevelse i Go.
