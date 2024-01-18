---
title:                "Scrivere test"
html_title:           "Lua: Scrivere test"
simple_title:         "Scrivere test"
programming_language: "Lua"
category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/writing-tests.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Scrivere test è un'attività fondamentale per i programmatori. Consiste nell'eseguire una serie di verifiche automatiche su parti di codice per verificare la correttezza del loro funzionamento. I programmatori lo fanno per garantire la qualità del loro codice e per ridurre il rischio di errori in produzione.

## Come fare:
Una delle librerie più popolari per scrivere test in Lua è [busted](https://olivinelabs.com/busted/). Con questa libreria, si possono scrivere test per ogni funzione o modulo del proprio codice. Ecco un esempio di test con busted per una semplice funzione che calcola il quadrato di un numero:

```Lua
-- Codice da testare
function square(x)
  return x * x
end

-- Test con busted
busted.describe("Square function", function()
  busted.it("Calculates the square of a number", function()
    busted.assert.equals(square(3), 9)
    busted.assert.equals(square(5), 25)
  end)
end)
```

L'output del test risulterà positivo se la funzione square calcolerà correttamente il quadrato dei numeri passati come argomento.

## Approfondimento:
Scrivere test è una pratica comune in tutti i linguaggi di programmazione ed è diventato particolarmente importante nell'ambito dello sviluppo agile e del test-driven development (TDD). Grazie ai test, possiamo avere la sicurezza che il nostro codice funzioni correttamente e ci permette di fare cambiamenti con maggiore facilità.

Un'alternativa a busted è [luaunit](https://github.com/bluefoot/luaunit), che si basa sulle asserzioni di [JUnit](https://junit.org/). Inoltre, si possono anche utilizzare le asserzioni fornite dalla libreria di base di Lua.

Per implementare i test nel proprio codice, è importante seguire alcune best practice come scrivere test atomici, il più possibile indipendenti tra loro, e utilizzare dati di test di input diversi per coprire tutti i casi possibili.

## Vedi anche:
- [busted](https://olivinelabs.com/busted/): Libreria per scrivere test in Lua
- [luaunit](https://github.com/bluefoot/luaunit): Alternativa a busted basata su JUnit
- [JUnit](https://junit.org/): Libreria per scrivere test in Java