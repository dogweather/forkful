---
title:                "Utilizzo delle espressioni regolari"
html_title:           "Lua: Utilizzo delle espressioni regolari"
simple_title:         "Utilizzo delle espressioni regolari"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Che cos'è & Perché usarle?

Le espressioni regolari, o "regex" per gli amici, sono una forma di sintassi utilizzata dai programmatori per cercare e manipolare stringhe di testo in modo preciso e veloce. Vengono spesso utilizzate per cercare determinati pattern all'interno di un testo o per effettuare sostituzioni automatiche. I programmatori utilizzano le espressioni regolari perché offrono una soluzione più efficace rispetto all'uso di metodi di ricerca e manipolazione stringhe tradizionali.

## Come si usano:

```Lua
-- Controlla se la parola "ciao" è presente in una stringa
if string.match("Ciao a tutti!", "ciao") ~= nil then
    print("La stringa contiene la parola ciao")
else
    print("La stringa non contiene la parola ciao")
end

-- Sostituisci ogni vocale con una "x"
print(string.gsub("Ciao a tutti!", "[aeiou]", "x"))
-- Output: Cxx xm txxxtx!

-- Cerca e salva un numero di telefono in una stringa
local phone_number = string.match("Il mio numero di telefono è 123-456-7890", "%d%d%d%-%d%d%d%-%d%d%d%d")
print("Il numero di telefono è " .. phone_number)
-- Output: Il numero di telefono è 123-456-7890
```

## Approfondimento:

Le espressioni regolari sono state inventate negli anni '40 da un matematico americano e successivamente sviluppate e rese popolari da linguaggi di programmazione come Perl e Java. Una loro alternativa più moderna è rappresentata dalle "lingue di templating", come Mustache e Handlebars, che offrono delle sintassi più intuitive e leggibili per manipolare dati testuali. Le espressioni regolari vengono implementate nella maggior parte dei linguaggi di programmazione, incluso Lua, tramite l'utilizzo di librerie apposite.

## Vedi anche:

Per un approfondimento sulle espressioni regolari in Lua e per imparare i vari comandi e shortcut disponibili, si consiglia di consultare la documentazione ufficiale sul sito di Lua. Inoltre, ci sono molti tutorial e video online che illustrano l'utilizzo delle espressioni regolari in linguaggi di programmazione diversi, che possono essere utili per approfondire le proprie conoscenze su questo argomento.