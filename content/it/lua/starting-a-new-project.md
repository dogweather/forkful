---
title:                "Iniziare un nuovo progetto"
html_title:           "Arduino: Iniziare un nuovo progetto"
simple_title:         "Iniziare un nuovo progetto"
programming_language: "Lua"
category:             "Lua"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?

Un nuovo progetto di programmazione si riferisce all'atto di iniziare a scrivere codice per una nuova applicazione o funzionalità. Lo facciamo perché dobbiamo costruire qualcosa o risolvere un problema esistente.

## Come fare?

In Lua, per avviare un semplice progetto, non abbiamo bisogno di configurazioni complesse:

```Lua
print("Benvenuti nel mio nuovo progetto!")
```

E alla fine invocando `lua nomefile.lua`, vediamo l'output:

```
Benvenuti nel mio nuovo progetto!
```

Direi nulla di complesso, si? Ora se necessitiamo di un ciclo:

```Lua 
for i = 1, 5 do 
    print("Sto lavorando al mio nuovo progetto: "..i) 
end
```

Con l'output:

```
Sto lavorando al mio nuovo progetto: 1
Sto lavorando al mio nuovo progetto: 2
Sto lavorando al mio nuovo progetto: 3
Sto lavorando al mio nuovo progetto: 4
Sto lavorando al mio nuovo progetto: 5
```

## Approfondimento 

Lua è appare nei tardi anni '90 come linguaggio scripting leggero per applicazioni incapsulate. A differenza di linguaggi come Python o Ruby, Lua non ha una batteria inclusa. Questo significa che dovrai probabilmente cercare o costruire le tue librerie per attività più complesse.

Considera, per esempio, l'HTTP. In Python, esiste il modulo `requests`. In Lua, dovrai probabilmente cercare una libreria di terze parti o scrivere la tua implementazione. Questo può essere una sfida o un'opportunità, a seconda della tua prospettiva.

Detto questo, ci sono numerosi moduli Lua disponibili e molte implementazioni hanno librerie standard estese (come LuaJIT). Inoltre, Lua possiede un ottimo sistema per l'estendibilità C, quindi se hai del codice C esistente che vorresti sfruttare, Lua potrebbe essere una buona scelta.

## Vedi Anche

- [Documentazione ufficiale Lua](http://www.lua.org/manual/5.3/)
- [Programmazione in Lua](http://www.lua.org/pil/)
- [Awesome Lua](https://github.com/LewisJEllis/awesome-lua), una lista curata di fantastiche librerie e software Lua.