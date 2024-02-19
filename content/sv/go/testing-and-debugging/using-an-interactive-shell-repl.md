---
aliases:
- /sv/go/using-an-interactive-shell-repl/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:23.439501-07:00
description: "En interaktiv skal, eller Read-Eval-Print Loop (REPL), l\xE5ter dig\
  \ experimentera med Go-kod i realtid, genom att k\xF6ra kommandon och f\xE5 omedelbar\u2026"
lastmod: 2024-02-18 23:08:51.335080
model: gpt-4-0125-preview
summary: "En interaktiv skal, eller Read-Eval-Print Loop (REPL), l\xE5ter dig experimentera\
  \ med Go-kod i realtid, genom att k\xF6ra kommandon och f\xE5 omedelbar\u2026"
title: "Anv\xE4nda en interaktiv skal (REPL)"
---

{{< edit_this_page >}}

## Vad & Varför?

En interaktiv skal, eller Read-Eval-Print Loop (REPL), låter dig experimentera med Go-kod i realtid, genom att köra kommandon och få omedelbar återkoppling. Denna metod är mycket använd för lärande, felsökning och prototypning, eftersom den kringgår den traditionella redigera-kompilera-köra-cykeln, vilket gör utvecklingsprocessen snabbare och mer intuitiv.

## Hur man gör:

Även om Go inte inkluderar en inbyggd REPL, har gemenskapen skapat verktyg som `gore` för att fylla detta gap. Först, installera `gore` genom att köra:

```
$ go get -u github.com/motemen/gore
```

När det är installerat, starta `gore` genom att skriva `gore` i din terminal:

```
$ gore
```

Du bör se en prompt redo att ta emot Go-kommandon. Låt oss prova ett enkelt exempel:

```
gore> :import fmt
gore> fmt.Println("Hej, Go REPL!")
```

Du skulle se utskriften som:

```
Hej, Go REPL!
```

Variabler och funktionsdefinitioner fungerar som förväntat. Du kan deklarera en funktion:

```
gore> :import math
gore> areaCircle := func(radius float64) float64 {
...> return math.Pi * radius * radius
...> }
gore> fmt.Println("Area av cirkel med radien 4:", areaCircle(4))
```

Och få utskriften direkt:

```
Area av cirkel med radien 4: 50.26548245743669
```

## Fördjupning:

Konceptet med en REPL är uråldrigt, som går tillbaka till Lisp-maskinerna på 1960-talet, och ger en interaktiv programmeringsupplevelse. Till skillnad från språk som Python eller JavaScript, designades Go utan en REPL, med fokus istället på kompilerade binärer för prestanda och enkelhet. Detta återspeglar Gos filosofi om enkelhet och dess design för skalbar och underhållbar mjukvara.

Dock visar verktyg som `gore` eller `goplay` Gos gemenskaps resursfullhet i att överbrygga detta gap. Dessa verktyg tolkar Go-kod dynamiskt och använder paketet `go/eval` eller liknande mekanismer för att exekvera den i realtid, om än med vissa begränsningar jämfört med en infödd REPL-miljö. Dessa begränsningar kommer från Gos typsystem och kompileringsmodell, vilket kan göra omedelbar utvärdering utmanande.

Även om REPL-miljöer är exceptionellt användbara för utbildning och snabba tester, tenderar Go-ekosystemet vanligtvis att gravitera mot traditionella kompilera-och-köra-processer för de flesta utvecklingsuppgifter. IDE:s och redigerare med stöd för Go, som Visual Studio Code eller GoLand, erbjuder integrerade verktyg för testning och felsökning som lindrar mycket av behovet av en REPL för professionell utveckling.

För utforskande programmering, prototypning eller lärande erbjuder dock REPL:er som `gore` ett värdefullt alternativ, som gör det möjligt för programmerare som är vana vid REPL:er i andra språk att njuta av en liknande upplevelse i Go.
