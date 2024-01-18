---
title:                "Analisi di HTML"
html_title:           "Lua: Analisi di HTML"
simple_title:         "Analisi di HTML"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/parsing-html.md"
---

{{< edit_this_page >}}

## Che cosa e Perché?

Parsing HTML è il processo di analizzare un documento HTML estrarre le informazioni incorporate. I programmatori spesso lo fanno per manipolare e visualizzare le informazioni in modo più user-friendly.

## Come fare:

### Esempio 1:
Input:
```Lua
local html = [[
<html>
  <head>
    <title>Titolo della Pagina</title>
  </head>
  <body>
    <h1>Benvenuti!</h1>
    <p>Questo è un paragrafo di testo.</p>
  </body>
</html>
]]
local dom = require("dom")
local document = dom.parse(html)

print(document.head.title) -- output: Titolo della Pagina
print(document.body.children[1].textContent) -- output: Benvenuti!
print(document.body.children[2].textContent) -- output: Questo è un paragrafo di testo.
```

### Esempio 2:
Input:
```Lua
local html = [[
<html>
  <head>
    <title>Titolo della Pagina</title>
  </head>
  <body>
    <h1>Sali</h1>
    <ul>
      <li>Mangia</li>
      <li>Dormi</li>
      <li>Ripeti</li>
    </ul>
  </body>
</html>
]]
local dom = require("dom")
local document = dom.parse(html)

print(document.head.title) -- output: Titolo della Pagina
print(document.body.children[1].textContent) -- output: Sali
print(document.body.children[2].children[2].textContent) -- output: Dormi
print(document.body.children[2].children[3].textContent) -- output: Ripeti
```

## Deep Dive:

### Contesto Storico:
Parsing HTML è stato introdotto nel 1993 da Tim Berners-Lee, l'inventore del World Wide Web. Inizialmente, questo processo era fatto manualmente dai programmatori, ma con l'avanzamento della tecnologia, sono stati sviluppati strumenti appositi per automatizzarlo.

### Alternative:
Un'alternativa al parsing HTML è l'utilizzo di librerie come BeautifulSoup in Python o jsdom in JavaScript. Invece, nelle applicazioni web basate su Lua, spesso viene utilizzata la libreria Lua HTML che offre funzionalità simili.

### Dettagli di Implementazione:
Il processo di parsing di solito viene fatto in tre fasi: analisi, costruzione dell'albero e manipolazione dell'albero. La libreria dom implementa queste fasi e permette di accedere alle informazioni tramite la struttura ad albero dell'HTML.

## Vedi anche:

- [Lua HTML](https://github.com/msva/lua-html)
- [BeautifulSoup](https://www.crummy.com/software/BeautifulSoup/)
- [jsdom](https://github.com/jsdom/jsdom)