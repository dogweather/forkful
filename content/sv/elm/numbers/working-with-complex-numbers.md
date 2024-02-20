---
date: 2024-01-26 04:39:35.364350-07:00
description: "Komplexa tal \xE4r en kombination av reella och imagin\xE4ra tal, som\
  \ `a + bi` d\xE4r `i` \xE4r kvadratroten av -1. De \xE4r avg\xF6rande inom omr\xE5\
  den som\u2026"
lastmod: 2024-02-19 22:04:57.034506
model: gpt-4-0125-preview
summary: "Komplexa tal \xE4r en kombination av reella och imagin\xE4ra tal, som `a\
  \ + bi` d\xE4r `i` \xE4r kvadratroten av -1. De \xE4r avg\xF6rande inom omr\xE5\
  den som\u2026"
title: Att arbeta med komplexa tal
---

{{< edit_this_page >}}

## Vad och varför?
Komplexa tal är en kombination av reella och imaginära tal, som `a + bi` där `i` är kvadratroten av -1. De är avgörande inom områden som ingenjörsvetenskap och fysik för att lösa problem som vanliga tal inte kan hantera.

## Hur man gör:
Elm har inte inbyggt stöd för komplexa tal, så du kommer att skapa din egen typ och funktioner. Här är en snabb uppsättning:

```Elm
type alias Complex =
    { real : Float, imaginary : Float }

add : Complex -> Complex -> Complex
add a b =
    { real = a.real + b.real, imaginary = a.imaginary + b.imaginary }

-- Exempelanvändning:
a = { real = 3, imaginary = 2 }
b = { real = 1, imaginary = -4 }

sum = add a b
-- summan är { real = 4.0, imaginary = -2.0 }
```

## Djupdykning
Historiskt sett accepterades inte alltid komplexa tal. De blev en spelväxlare på 1500-talet för att lösa kubiska ekvationer. Alternativ i andra språk som Python erbjuder inbyggt stöd för komplexa tal med operationer direkt ur lådan. Elm kräver ett gör-det-själv-angreppssätt som du sett. Men du kan göra det så sofistikerat som behövs, bygga multiplikation, division och andra operationer, finjustera prestandaproblem.

## Se även
- Elms officiella dokumentation: https://package.elm-lang.org/ för att skapa anpassade typer och bemästra Elm-grunderna.
- Matematikhistorieentusiaster skulle kunna kolla in "An Imaginary Tale" av Paul J. Nahin för en resa genom de komplexa talens tid.
- Dyk ner i matematikorienterade programmeringsutmaningar på Project Euler (https://projecteuler.net) för att tillämpa din komplexa tal-trollkonst.
