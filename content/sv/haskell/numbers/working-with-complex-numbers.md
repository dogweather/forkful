---
aliases:
- /sv/haskell/working-with-complex-numbers/
date: 2024-01-26 04:41:45.423847-07:00
description: "Komplexa tal, som best\xE5r av en reell och en imagin\xE4r del, \xE4\
  r grundl\xE4ggande inom olika ber\xE4kningsf\xE4lt som teknik, fysik och signalbehandling.\
  \ Programmerare\u2026"
lastmod: 2024-02-18 23:08:51.831549
model: gpt-4-0125-preview
summary: "Komplexa tal, som best\xE5r av en reell och en imagin\xE4r del, \xE4r grundl\xE4\
  ggande inom olika ber\xE4kningsf\xE4lt som teknik, fysik och signalbehandling. Programmerare\u2026"
title: Att arbeta med komplexa tal
---

{{< edit_this_page >}}

## Vad & Varför?

Komplexa tal, som består av en reell och en imaginär del, är grundläggande inom olika beräkningsfält som teknik, fysik och signalbehandling. Programmerare använder dem för att lösa ekvationer som reella tal inte kan, som att hitta rötter till negativa tal.

## Hur man gör:

Haskell hanterar komplexa tal med modulen `Data.Complex`. Här är en snabb översikt:

```haskell
import Data.Complex

-- Definiera två komplexa tal
let z1 = 3 :+ 4  -- det är 3 + 4i
let z2 = 5 :+ (-2)  -- 5 - 2i

-- Aritmetiska operationer
let sum = z1 + z2  -- 8 :+ 2
let skillnad = z1 - z2  -- -2 :+ 6
let produkt = z1 * z2  -- 23 :+ 14
let kvot = z1 / z2  -- 0.20689655172413793 :+ 0.9655172413793104

-- Komplex konjugat
let konjugatZ1 = conjugate z1  -- 3 :+ (-4)

-- Magnitud och fas
let magnitudZ1 = magnitude z1  -- 5.0
let fasZ1 = phase z1  -- 0.9272952180016122

-- Omvandling mellan polär och rektangulär form och vice versa
let z1Polar = polar z1  -- (5.0,0.9272952180016122)
let franPolar = mkPolar 5.0 0.9272952180016122  -- samma som z1
```

Exempelutdata efter att ha laddat ovanstående kod i GHCi kan vara:

```haskell
*Main> sum
8.0 :+ 2.0
*Main> produkt
23.0 :+ 14.0
*Main> magnitudZ1
5.0
```

## Djupdykning

Komplexa tal går tillbaka till 1500-talet men accepterades i större utsträckning mycket senare. Haskell, liksom många andra språk, ger inföddt stöd för komplex aritmetik, vilket gör det enkelt att arbeta med dessa tal utan att implementera den underliggande matematiken.

Alternativ inkluderar att bygga din egen komplexa taltyp eller använda bibliotek för specifika domäner såsom kvaternioner för 3D-grafik. Men för de flesta användningsfall är Haskell:s `Data.Complex` mer än tillräckligt.

I bakgrunden är `Data.Complex` bara en datatyp som parar ihop två `Float` eller `Double` värden, som representerar den reella och imaginära delen, respektive. Det är ett enkelt och effektivt sätt att arbeta med komplexa tal på Haskell-plattformen.

## Se också

Kolla in dessa resurser för mer om komplexa tal i Haskell:

- Den officiella Haskell `Data.Complex` dokumentationen: [Hackage Data.Complex](https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Complex.html)
- En djupdykning i Haskells taltyper: [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/starting-out#numbers)
- För en tillämpning, utforska algoritmer för snabb Fourier-transform i Haskell: [Haskell FFT-bibliotek](https://hackage.haskell.org/package/fft)
