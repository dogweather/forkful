---
title:                "Generazione di numeri casuali."
html_title:           "Lua: Generazione di numeri casuali."
simple_title:         "Generazione di numeri casuali."
programming_language: "Lua"
category:             "Lua"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Generare numeri casuali è un'operazione comune nei programmi per computer. Può essere utilizzata per scopi come la generazione di password, la creazione di giochi o l'esecuzione di test casuali per il debug del codice.

## Come fare:
```
-- Generare un numero casuale compreso tra 1 e 10
math.randomseed( os.time() )	-- Imposta il seme del generatore di numeri casuali con l'ora di sistema attuale
print(math.random(1,10))	-- Stampa un numero casuale compreso tra 1 e 10

-- Generare una lista di numeri casuali
math.randomseed( os.time() )
for i = 1, 5 do	-- Ripeti 5 volte
   print(math.random())	-- Stampa un numero casuale (con decimali) compreso tra 0 e 1
end
```

Output:
```
6
0.3557946601246
0.26770305847322
0.97415052113529
0.76716162791216
0.44311839629823
```

## Approfondimento:
La generazione di numeri casuali è una tecnica che risale ai primi tempi della programmazione dei computer. In passato, i programmi utilizzavano metodi matematici per simulare l'aleatorietà, ma questi metodi erano prevedibili e quindi non sempre utili. Con l'avvento dei computer moderni, è stato possibile utilizzare algoritmi che sfruttano il calcolo dei numeri pseudocasuali, basati su un seme generato dall'ora di sistema o da altri dati imprevedibili. Esistono anche librerie esterne, scritte in altri linguaggi di programmazione, che forniscono funzioni avanzate per la generazione di numeri casuali.

## Vedi anche:
- [Documentazione ufficiale di Lua sulla generazione di numeri casuali](https://www.lua.org/manual/5.3/manual.html#6.6)
- [Esempi di utilizzo di numeri casuali in giochi con Lua](https://www.gammon.com.au/rolls)
- [Libreria esterna 'math.random' per la generazione di numeri casuali](https://github.com/KeplerProject/luarocks/blob/master/math-random-scm-1.rockspec)