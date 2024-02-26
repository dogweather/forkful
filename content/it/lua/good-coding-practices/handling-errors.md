---
date: 2024-01-26 00:55:28.746706-07:00
description: "La gestione degli errori nella programmazione riguarda l'anticipare\
  \ l'inaspettato. \xC8 l'arte di pianificare per quando le cose non vanno come previsto\
  \ cos\xEC\u2026"
lastmod: '2024-02-25T18:49:41.427550-07:00'
model: gpt-4-1106-preview
summary: "La gestione degli errori nella programmazione riguarda l'anticipare l'inaspettato.\
  \ \xC8 l'arte di pianificare per quando le cose non vanno come previsto cos\xEC\u2026"
title: Gestione degli errori
---

{{< edit_this_page >}}

## Cosa & Perché?
La gestione degli errori nella programmazione riguarda l'anticipare l'inaspettato. È l'arte di pianificare per quando le cose non vanno come previsto così da poter mantenere il tuo programma funzionante in modo fluido.

## Come fare:
Lua utilizza due funzioni principali per la gestione degli errori: `pcall` e `xpcall`. Ecco come si usano:

```lua
function might_fail()
    if math.random() > 0.5 then
        error("Ops! Qualcosa è andato storto.")
    else
        print("Tutto bene!")
    end
end

-- Utilizzando pcall
local successo, messaggioErrore = pcall(might_fail)

if successo then
    print("Successo!")
else
    print("Errore intercettato:", messaggioErrore)
end

-- Utilizzando xpcall con una funzione di gestione degli errori
function myErrorHandler(err)
    print("Gestore degli errori dice:", err)
end

local stato = xpcall(might_fail, myErrorHandler)
print("La chiamata è stata successful?", stato)
```

Un esempio di output potrebbe essere:

```
Errore intercettato: Ops! Qualcosa è andato storto.
Gestore degli errori dice: Ops! Qualcosa è andato storto.
La chiamata è stata successful? false
```
Oppure, se non si verificano errori:
```
Tutto bene!
Successo!
Tutto bene!
La chiamata è stata successful? true
```

## Approfondimento
La gestione degli errori, o "exception handling", non è sempre stata una pratica. I programmi precedenti si bloccavano spesso. Con l'evoluzione della programmazione, è aumentata anche la necessità di stabilità. L'approccio di Lua è semplice rispetto ad alcuni linguaggi. Non ci sono blocchi `try/catch`, solo `pcall` e `xpcall`. Il primo protegge una chiamata di funzione, restituendo uno stato e un eventuale errore. Il secondo aggiunge una funzione di gestione degli errori, utile per una pulizia personalizzata o per il logging.

Un'alternativa in Lua è utilizzare `assert`, che può servire uno scopo simile lanciando un errore se la sua condizione è falsa. Ma non è flessibile come `pcall` per scenari di gestione degli errori complessi.

Internamente, `pcall` e `xpcall` funzionano impostando un "ambiente protetto" per l'esecuzione della funzione. Se si verifica un errore, l'ambiente lo intercetta e può gestirlo immediatamente o passarlo indietro affinché il programma lo gestisca.

## Vedi Anche
- Il libro "Programming in Lua" (terza edizione), disponibile su https://www.lua.org/pil/ per un approfondimento completo sulla gestione degli errori (Sezione 8.4).
- Manuale di Riferimento Ufficiale Lua 5.4: https://www.lua.org/manual/5.4/ - per le informazioni più aggiornate sulle funzioni di gestione degli errori di Lua.
- Wiki degli utenti Lua sulla gestione degli errori: http://lua-users.org/wiki/ErrorHandling – per approfondimenti e pattern della comunità.
