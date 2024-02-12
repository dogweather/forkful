---
title:                "Capitalizzare una stringa"
aliases:
- /it/lua/capitalizing-a-string.md
date:                  2024-02-03T19:05:48.153814-07:00
model:                 gpt-4-0125-preview
simple_title:         "Capitalizzare una stringa"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cos'è & Perché?
Capitalizzare una stringa significa modificare il primo carattere di ogni parola in una frase per renderlo maiuscolo, garantendo che i restanti caratteri siano minuscoli. Questa tecnica è comunemente usata per formattare il testo rendendolo più professionale o leggibile, come ad esempio preparare titoli o input degli utenti per la visualizzazione.

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
