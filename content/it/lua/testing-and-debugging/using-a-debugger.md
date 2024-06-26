---
date: 2024-01-26 03:50:20.013557-07:00
description: "Come fare: Lua non \xE8 dotato di un debugger integrato, ma \xE8 possibile\
  \ utilizzare debugger esterni, come ZeroBrane Studio. Ecco un assaggio di come si\u2026"
lastmod: '2024-03-13T22:44:43.562097-06:00'
model: gpt-4-0125-preview
summary: "Lua non \xE8 dotato di un debugger integrato, ma \xE8 possibile utilizzare\
  \ debugger esterni, come ZeroBrane Studio."
title: Utilizzo di un debugger
weight: 35
---

## Come fare:
Lua non è dotato di un debugger integrato, ma è possibile utilizzare debugger esterni, come ZeroBrane Studio. Ecco un assaggio di come si lavora con esso:

```Lua
-- Questo è un semplice script Lua con un errore intenzionale
local function add(a, b)
    local result = a+ b -- Oops, facciamo finta di aver dimenticato di definire 'b'
    return result
end

print(add(10))
```

Quando esegui questo codice in un debugger, l'esecuzione si interrompe dove le cose si incasinano. Vedrai qualcosa del genere:

```
lua: example.lua:3: tentativo di eseguire un'aritmetica su un valore nil (locale 'b')
traccia dello stack:
	example.lua:3: nella funzione 'add'
	example.lua:7: nel blocco principale
	[C]: in ?
```

Puoi impostare breakpoint, passare attraverso il tuo codice passo dopo passo e esaminare i valori delle variabili per rintracciare il bug senza perdere la testa.

## Approfondimento
Purtroppo, la semplicità di Lua non si estende al debugging. Niente paura, però, la comunità di Lua ti copre le spalle. Strumenti come ZeroBrane Studio, LuaDec e altri offrono capacità di debugging. Storicamente, i debugger esistevano poco dopo che i primi programmi iniziavano a presentare problemi, fornendo agli sviluppatori i mezzi per correggere il loro codice senza agire alla cieca.

Con Lua, spesso si fa affidamento su debugger esterni o si integrano nel proprio ambiente di sviluppo. ZeroBrane Studio, ad esempio, è un IDE che integra completamente un debugger Lua. Ti consente di passare attraverso il codice passo dopo passo, impostare breakpoint e osservare variabili. Sul lato implementativo, i debugger utilizzano generalmente hook per inserire breakpoint e altre strutture di debug.

Alternative? Certo che sì. I vecchi e buoni statement `print`, affettuosamente noti come "debugging printf", possono a volte fare il trucco senza strumenti sofisticati.

## Vedi anche
Per continuare il tuo percorso di debugging, consulta:

- ZeroBrane Studio: https://studio.zerobrane.com/
- Wiki degli utenti Lua sul Debugging del Codice Lua: http://lua-users.org/wiki/DebuggingLuaCode
- Il riferimento della libreria `debug` nel manuale di Lua: https://www.lua.org/manual/5.4/manual.html#6.10
