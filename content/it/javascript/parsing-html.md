---
title:                "Parsing di Html"
html_title:           "Javascript: Parsing di Html"
simple_title:         "Parsing di Html"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/parsing-html.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

Il parsing HTML è il processo di analisi di un documento HTML. Ciò significa che il programma legge il codice sorgente HTML e lo trasforma in un formato più facilmente comprensibile e utilizzabile. I programmatori spesso fanno il parsing HTML per estrarre informazioni specifiche da una pagina web o per creare un'interfaccia utente dinamica.

## Come fare:

```Javascript
// Esempio di parsing HTML utilizzando la libreria Cheerio
const cheerio = require('cheerio');
const html = '<div><h1>Titolo pagina</h1><p>Descrizione della pagina</p></div>';
const $ = cheerio.load(html);
const title = $('h1').text();       // Ottieni il testo contenuto nell'elemento h1
const description = $('p').text();  // Ottieni il testo contenuto nell'elemento p
console.log(title);                 // Output: Titolo pagina
console.log(description);           // Output: Descrizione della pagina
```

## Approfondimento:

Il parsing HTML è stato introdotto nel 1993 da Tim Berners-Lee per creare il primo browser web, il WorldWideWeb. Oggi esistono varie alternative per il parsing HTML, tra cui la libreria htmlparser2 e l'utilizzo di espressioni regolari. L'implementazione del parsing HTML può variare a seconda del linguaggio di programmazione utilizzato, ma il concetto di base rimane lo stesso.

## Vedi anche:

- [Cheerio](https://cheerio.js.org/) - Un'efficace libreria per il parsing HTML in Javascript.
- [htmlparser2](https://github.com/fb55/htmlparser2) - Un parser HTML scritto in Javascript.
- [Regular expressions in Javascript](https://developer.mozilla.org/it/docs/Web/JavaScript/Guide/Regular_Expressions) - Informazioni su come utilizzare le espressioni regolari per il parsing HTML.