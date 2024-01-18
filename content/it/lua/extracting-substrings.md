---
title:                "Estrazione di sottostringhe"
html_title:           "Lua: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/extracting-substrings.md"
---

{{< edit_this_page >}}

\## Che cosa è e Perché?

L'estrazione di sottostringhe in Lua è il processo di selezionare una porzione di una stringa più grande in base ad un intervallo di caratteri specificato. I programmatori spesso eseguono questa operazione per ottenere una parte specifica di una stringa, come ad esempio una parola o una frase, per elaborarla ulteriormente.

\## Come fare:

```Lua
--Definiamo una stringa
nome = "Marco Rossi"

--Estraiamo la sottostringa dal primo carattere al quinto
sottostringa = nome:sub(1,5)
print(sottostringa) --output: Marco

--Estraiamo la sottostringa dal sesto carattere in poi
sottostringa = nome:sub(6)
print(sottostringa) --output: Rossi
```

\## Approfondimento:

La funzione ```sub``` per l'estrazione di sottostringhe è stata introdotta nella versione 5.1 di Lua, ma è diventata standard solo nella versione 5.2. In precedenza, i programmatori utilizzavano la funzione ```string.sub``` per ottenere lo stesso risultato. Un altro modo per estrarre una sottostringa è utilizzare la funzione ```string.match``` in combinazione con le espressioni regolari.

\## Vedi anche:

- [Documentazione ufficiale di Lua](https://www.lua.org/manual/5.3/manual.html#6.4)
- [Tutorial sull'estrazione di sottostringhe in Lua](https://www.lua.org/pil/20.2.html)
- [Approccio alla gestione delle stringhe in Lua](https://www.lua.org/manual/2.5/manual.html#6.4)