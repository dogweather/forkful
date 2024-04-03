---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:48.153814-07:00
description: "Capitalizzare una stringa significa modificare il primo carattere di\
  \ ogni parola in una frase per renderlo maiuscolo, garantendo che i restanti caratteri\u2026"
lastmod: '2024-03-13T22:44:43.542488-06:00'
model: gpt-4-0125-preview
summary: Capitalizzare una stringa significa modificare il primo carattere di ogni
  parola in una frase per renderlo maiuscolo, garantendo che i restanti caratteri
  siano minuscoli.
title: Capitalizzare una stringa
weight: 2
---

## Come fare:
Lua non dispone di una funzione incorporata per capitalizzare le stringhe, ma è possibile realizzare facilmente questo compito utilizzando funzioni basilari di manipolazione delle stringhe. Ecco una semplice funzione per capitalizzare la prima lettera di una singola parola:

```lua
function capitalize(word)
    return word:sub(1,1):upper() .. word:sub(2):lower()
end

print(capitalize("hello"))  -- Output: Hello
```

Per capitalizzare ogni parola in una frase, puoi dividere la frase in parole, capitalizzarle una ad una e poi riunirle:

```lua
function capitalizeSentence(sentence)
    local words = {}
    for word in sentence:gmatch("%S+") do
        table.insert(words, capitalize(word))
    end
    return table.concat(words, " ")
end

print(capitalizeSentence("hello world from lua"))  -- Output: Hello World From Lua
```

Se stai lavorando a un progetto dove la performance è fondamentale e ti ritrovi a necessitare capacità di manipolazione delle stringhe più avanzate, considera l'utilizzo di una libreria di terze parti come `Penlight`. Penlight migliora Lua con funzioni di gestione delle stringhe più versatili, tra le altre utilità:

```lua
-- Assumendo che Penlight sia installato:
local pl = require("pl.stringx")
local text = "hello lua users"
text = pl.capitalized(text)
print(text)  -- Output: Hello lua users

-- Nota: La funzione capitalized di Penlight capitalizza solo la prima parola.
-- Per capitalizzare ogni parola, dovrai comunque implementare una soluzione personalizzata o esplorare altre librerie.
```
