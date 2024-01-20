---
title:                "Eliminazione dei caratteri corrispondenti a un modello"
html_title:           "PowerShell: Eliminazione dei caratteri corrispondenti a un modello"
simple_title:         "Eliminazione dei caratteri corrispondenti a un modello"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Che Cosa e Perché?
Nei linguaggi di programmazione come Lua, l'eliminazione dei caratteri che corrispondono a un determinato modello è un'operazione comune. Questo è noto come "pattern matching". Questa operazione è essenziale per manipolare le stringhe, filtrare le informazioni e pulire i dati.

# Come Fare:
In Lua, puoi eliminare i caratteri dal modello utilizzando la funzione `gsub`. Ecco un esempio:

```Lua
stringaDaCambiare = "Ciao, amico!"
stringaModificata = string.gsub(stringaDaCambiare, "a", "") 
print(stringaModificata)
```

L'output è:

```Lua
"Cio, mico!"
```

In questo caso, tutte le occorrenze del carattere "a" sono state eliminate.

# Approfondimenti:
Il pattern matching ha una lunga storia nei linguaggi di programmazione, risalente a quelli come Perl e Bash. Lua ha adottato un sistema molto simile. 

Un'alternativa all'uso di `gsub` potrebbe essere il ciclo attraverso ogni carattere singolarmente e la costruzione di una nuova stringa, ma ciò è spesso meno efficiente.

In realtà, `gsub` nel suo interno, utilizza l'algoritmo dell'espressione regolare (o una variante) per abbinare i pattern, il che lo rende molto potente.

# Approfondire:
Se vuoi approfondire l'argomento, ecco alcuni link utili:

- [Lua Reference Manual](https://www.lua.org/manual/5.4/manual.html#6.4.1)
- [Programming in Lua](https://www.lua.org/pil/20.2.html)
- [Lua Pattern Match](https://riptutorial.com/lua/example/2031/pattern-matches-in-lua)