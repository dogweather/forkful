---
title:                "Cercare e sostituire testo."
html_title:           "Lua: Cercare e sostituire testo."
simple_title:         "Cercare e sostituire testo."
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

Cosa & Perché?
La ricerca e la sostituzione del testo sono due attività fondamentali nel mondo della programmazione. Questo processo consiste nell'individuare un determinato testo all'interno di un documento o di un programma e sostituirlo con un altro testo. I programmatori spesso devono fare questa operazione per modificare, correggere o aggiornare il loro codice.

Come fare:
Ecco un esempio di come effettuare la ricerca e la sostituzione del testo in Lua utilizzando la funzione "string.gsub ()". Nel seguente codice, cercheremo la parola "ciao" all'interno della variabile "stringa" e la sostituirà con "salve".

```Lua
stringa = "Salve mondo, ciao!"
nuova_stringa = string.gsub(stringa, "ciao", "salve")
print(nuova_stringa)
```

L'output di questo codice sarà: "Salve mondo, salve!" Come puoi vedere, la funzione "string.gsub ()" ha trovato la parola "ciao" all'interno della stringa e l'ha sostituita con "salve". 

Un'altra opzione per effettuare la ricerca e la sostituzione del testo in Lua è utilizzare l'espressione regolare. Un'espressione regolare è uno strumento di ricerca avanzato che consente di cercare e sostituire testo basandosi su determinati pattern. Ad esempio, per cercare e sostituire solo le parole che iniziano con la lettera "c", puoi utilizzare l'espressione regolare "^c%w+".

```Lua
stringa = "ciao a tutti, come state?"
nuova_stringa = string.gsub(stringa, "^c%w+", "salve")
print(nuova_stringa)
```

L'output di questo codice sarà: "salve a tutti, come state?" Come puoi vedere, è stato effettuato solo il cambio di "ciao" in "salve" perché era l'unica parola che iniziava con "c" seguita da altre lettere.

Approfondimento:
La ricerca e la sostituzione del testo sono state introdotte nei primi linguaggi di programmazione come una funzionalità per semplificare la modifica del codice. Nel corso degli anni, sono state sviluppate molte librerie e funzioni per gestire in modo più avanzato la ricerca e la sostituzione del testo, come, ad esempio, la libreria "lpeg" che supporta l'utilizzo di grammatiche di espressioni regolari.

Inoltre, esistono anche altri strumenti per la ricerca e la sostituzione del testo come i text editor avanzati o gli script per l'automazione della modifica di più file di codice.

Vedi anche:
- Documentazione ufficiale di Lua sulla funzione "string.gsub ()": https://www.lua.org/manual/5.3/manual.html#pdf-string.gsub
- Tutorial sulle espressioni regolari in Lua: https://learnxinyminutes.com/docs/it-it/lua-it/
- Libreria "lpeg" per espressioni regolari avanzate in Lua: http://www.inf.puc-rio.br/~roberto/lpeg/