---
title:                "Interpolazione di una stringa"
html_title:           "Clojure: Interpolazione di una stringa"
simple_title:         "Interpolazione di una stringa"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/interpolating-a-string.md"
---

{{< edit_this_page >}}

---

## Che Cos'è e Perché?

L'interpolazione delle stringhe in Lua ti consente di inserire variabili direttamente all'interno di una stringa. Favorisce la leggibilità del codice e rende più semplice il formato delle stringhe.

## Come Fare:

Ecco un esempio di come puoi fare l'interpolazione delle stringhe in Lua:

```Lua
nome = "Mario"
print(("Ciao, %s!"):format(nome))
```

Output:

```Lua
Ciao, Mario!
```

In questo esempio, `%s` è un segnaposto che viene sostituito dal valore della variabile `nome`.

## Approfondimenti 

L'interpolazione delle stringhe ha una lunga storia in programmazione. Python, Ruby e altri linguaggi hanno anche supporto per l'interpolazione delle stringhe, sebbene la sintassi possa variare. In Lua, l'interpolazione delle stringhe si basa su una versione modificata del comando di formato `printf` del linguaggio C.

Esiste anche una libreria chiamata `stringy` che offre un approccio simile a Ruby o Python per l'interpolazione delle stringhe, ma non fa parte del core di Lua.

Dettagli di implementazione: la funzione `format` di Lua prende come input una stringa con segnaposti (`%s`, `%d`, ecc.) e un elenco di variabili. Ogni segnaposto viene sostituito con il valore corrispondente della variabile.

## Vedere Anche:

Per ulteriori informazioni sull'interpolazione delle stringhe in Lua, consulta le seguenti risorse:

- [The Programming in Lua book](https://www.lua.org/pil/20.2.html)
- [The Lua Users Wiki](http://lua-users.org/wiki/StringInterpolation)
- [The Lua 5.3 Reference Manual](https://www.lua.org/manual/5.3/manual.html#pdf-string.format)