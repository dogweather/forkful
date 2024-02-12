---
title:                "Lettura degli argomenti della riga di comando"
aliases:
- /it/lua/reading-command-line-arguments.md
date:                  2024-01-20T17:56:31.334649-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lettura degli argomenti della riga di comando"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa e Perché?)

Leggere gli argomenti della riga di comando in Lua significa estrarre i dati inseriti dall'utente quando avvia il tuo script. I programmatori fanno questo per personalizzare l'esecuzione di un programma dall'esterno, rendendolo più flessibile e interattivo.

## How to: (Come fare:)

```Lua
-- Salva il file come `echo_args.lua`
for i=1, #arg do
  print(i, arg[i])
end
```

Esegui lo script con: `lua echo_args.lua ciao mondo`
Output:

```
1    ciao
2    mondo
```

## Deep Dive (Approfondimento)

Lua gestisce gli argomenti della riga di comando con una tabella globale chiamata `arg`. Il primo elemento (indice 0) è il nome dello script. Gli elementi successivi sono gli argomenti. Lua è portatile e semplice, rendendolo perfetto per script di utilità. Alternativamente, per applicazioni più complesse, puoi usare librerie come `Penlight` o `pl.utils`, che offrono una gestione avanzata degli input da riga di comando. Nell'uso storico, leggere da riga di comando era essenziale per interagire con programmi in ambienti a bassa interazione grafica.

## See Also (Vedi Anche)

- [Argomenti della Riga di Comando in Lua](http://www.lua.org/pil/25.3.html)
- [Documentazione Ufficiale di Lua](http://www.lua.org/manual/5.4/)
- [Penlight GitHub Repository](https://github.com/lunarmodules/Penlight)
