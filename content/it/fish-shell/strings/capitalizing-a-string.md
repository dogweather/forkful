---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:21.818799-07:00
description: "Capitalizzare una stringa significa modificarla in modo che la prima\
  \ lettera sia maiuscola e il resto della stringa sia minuscolo. Questo \xE8 un compito\u2026"
lastmod: 2024-02-19 22:05:02.912452
model: gpt-4-0125-preview
summary: "Capitalizzare una stringa significa modificarla in modo che la prima lettera\
  \ sia maiuscola e il resto della stringa sia minuscolo. Questo \xE8 un compito\u2026"
title: Capitalizzare una stringa
---

{{< edit_this_page >}}

## Cosa & Perché?

Capitalizzare una stringa significa modificarla in modo che la prima lettera sia maiuscola e il resto della stringa sia minuscolo. Questo è un compito comune nell'elaborazione del testo, nella normalizzazione degli input degli utenti e nella formattazione dei dati per garantire coerenza o per soddisfare criteri di formattazione specifici.

## Come fare:

In Fish Shell, le stringhe possono essere manipolate direttamente con funzioni incorporate, senza la necessità di strumenti esterni o librerie. Per capitalizzare una stringa, puoi combinare il comando `string` con sottocomandi.

```fish
# Stringa di esempio
set sample_string "hello world"

# Capitalizza la prima lettera
set capitalized_string (string sub -l 1 -- $sample_string | string upper)(string sub -s 2 -- $sample_string)

echo $capitalized_string
```

Output:
```
Hello world
```

Per scenari che richiedono la capitalizzazione di più parole in una stringa (ad es., convertendo "hello world" in "Hello World"), si dovrebbe iterare su ogni parola, applicando la logica di capitalizzazione a ciascuna:

```fish
# Frase di esempio
set sentence "hello fish shell programming"

# Capitalizza ogni parola
set capitalized_words (string split " " -- $sentence | while read -l word; string sub -l 1 -- $word | string upper; and string sub -s 2 -- $word; end)

# Unisci le parole capitalizzate
set capitalized_sentence (string join " " -- $capitalized_words)

echo $capitalized_sentence
```

Output:
```
Hello Fish Shell Programming
```

Nota che Fish Shell non offre direttamente un approccio con un unico comando per la capitalizzazione di intere frasi nello stesso modo in cui alcuni linguaggi di programmazione fanno con i loro metodi di stringa. Quindi, combinare `string split`, `string sub`, `string upper`, e poi riunire rappresenta un approccio idiomatico in Fish Shell per raggiungere questo scopo.
