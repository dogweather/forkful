---
title:                "Analisi dell'HTML"
date:                  2024-01-20T15:32:42.901630-07:00
html_title:           "Bash: Analisi dell'HTML"
simple_title:         "Analisi dell'HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa e Perché?)
Il parsing di HTML significa estrarre dati da una pagina web. I programmatori lo fanno per automatizzare la raccolta di informazioni, come i prezzi dei prodotti o i titoli delle news.

## How to: (Come Fare)
Lua non ha una libreria standard per il parsing di HTML, quindi dobbiamo usare una esterna come `lua-html` o `luascrape`. Qui un esempio con `lua-html`:

```Lua
local html = require("html")

-- Caricare l'HTML da una stringa (si può anche usare html.parseFile per caricare da file)
local doc = html.parse("<html><head><title>Prova</title></head><body><p>Ciao, mondo!</p></body></html>")

-- Trovare il titolo della pagina
local title = doc:select("title")[1]
print(title:getcontent())  -- Output: Prova

-- Trovare tutti i paragrafi
for _, p in ipairs(doc:select("p")) do
    print(p:getcontent())  -- Output: Ciao, mondo!
end
```

## Deep Dive (Approfondimento)
Il parsing di HTML in Lua non è built-in: devi affidarti a librerie di terze parti. Fino a poco tempo fa, la comunità Lua mancava di una solida libreria HTML, spingendo gli sviluppatori verso soluzioni come l'espressioni regolari, che possono essere inefficienti e inaffidabili per questo scopo. Le moderne librerie, come `lua-html`, hanno colmato questa lacuna. Un'alternativa è `luascrape`, che facilita il web scraping. Tenete presente che il parsing dipenderà dalla correttezza dell'HTML; HTML malformato potrebbe richiedere una pre-elaborazione.

## See Also (Vedi Anche)
- Documentazione ufficiale Lua: [Lua Official Documentation](https://www.lua.org/manual/5.4/)
