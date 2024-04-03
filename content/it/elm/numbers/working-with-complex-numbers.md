---
date: 2024-01-26 04:39:24.822224-07:00
description: "I numeri complessi sono una combinazione di numeri reali e immaginari,\
  \ come `a + bi` dove `i` \xE8 la radice quadrata di -1. Sono fondamentali in campi\
  \ come\u2026"
lastmod: '2024-03-13T22:44:43.344187-06:00'
model: gpt-4-0125-preview
summary: "I numeri complessi sono una combinazione di numeri reali e immaginari, come\
  \ `a + bi` dove `i` \xE8 la radice quadrata di -1."
title: Lavorare con i numeri complessi
weight: 14
---

## Come fare:
Elm non ha un supporto integrato per i numeri complessi, quindi dovrai creare il tuo tipo e le tue funzioni. Ecco una configurazione rapida:

```Elm
type alias Complex =
    { real : Float, imaginary : Float }

add : Complex -> Complex -> Complex
add a b =
    { real = a.real + b.real, imaginary = a.imaginary + b.imaginary }

-- Esempio di utilizzo:
a = { real = 3, imaginary = 2 }
b = { real = 1, imaginary = -4 }

sum = add a b
-- sum è { real = 4.0, imaginary = -2.0 }
```

## Approfondimento
Storicamente, i numeri complessi non sono sempre stati accettati. Sono diventati una svolta nel XVI secolo per risolvere equazioni cubiche. Alternative in altri linguaggi come Python offrono supporto integrato per i numeri complessi con operazioni pronte all'uso. Elm richiede un approccio fai-da-te come hai visto. Ma puoi renderlo sofisticato quanto necessario, costruendo moltiplicazione, divisione e altre operazioni, affinando problemi di performance.

## Vedi Anche
- Documentazione Ufficiale di Elm: https://package.elm-lang.org/ per creare tipi personalizzati e padroneggiare le basi di Elm.
- Gli appassionati di storia della matematica potrebbero consultare "An Imaginary Tale" di Paul J. Nahin per un viaggio dei numeri complessi attraverso il tempo.
- Immergiti nelle sfide di programmazione orientate alla matematica su Project Euler (https://projecteuler.net) per applicare la tua magia sui numeri complessi.
