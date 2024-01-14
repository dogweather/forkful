---
title:                "Gleam: Scaricare una pagina web."
simple_title:         "Scaricare una pagina web."
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

Perché scaricare una pagina web può essere utile


Scaricare una pagina web può essere utile per diversi motivi: può permettere di avere una copia offline di una pagina web, di estrarre dati per analisi o semplicemente di avere accesso al contenuto in caso di perdita di connessione. Inoltre, con il linguaggio di programmazione Gleam, scaricare una pagina web può essere facilmente fatto seguendo pochi passaggi. 

Come farlo con Gleam

Per scaricare una pagina web con Gleam, è necessario importare il modulo `http` e utilizzare la funzione `get` passando come parametro l'URL della pagina da scaricare. Ad esempio: 

```Gleam
import http

let response = http.get("https://example.com")
```

La variabile `response` conterrà la risposta ottenuta dal server. Per ottenere il codice HTML della pagina, è possibile utilizzare la funzione `body` sul campo `response.body`, come mostrato nell'esempio seguente: 

```Gleam
let html = response.body.body
```

Per ulteriori informazioni sulla struttura della risposta e sui possibili errori, si consiglia di consultare la documentazione ufficiale del modulo `http`. 

Approfondimento

Scaricare una pagina web non è sempre una semplice operazione di ottenimento di un file HTML. Spesso, infatti, è necessario gestire richieste asincrone, autenticazioni o estrarre informazioni specifiche dalla pagina. Con Gleam, è possibile utilizzare librerie come `html_parser` per parsare il codice HTML e `xml` per estrarre informazioni da documenti XML. Inoltre, utilizzando il modulo `javascript`, è possibile eseguire codice JavaScript all'interno della pagina scaricata. 

See Also

- Documentazione ufficiale del modulo `http`: https://gleam.run/modules/http.html
- Libreria `html_parser` per il parsing di codice HTML: https://github.com/midas-framework/html_parser
- Modulo `javascript` per l'esecuzione di codice JavaScript: https://gleam.run/modules/javascript.html