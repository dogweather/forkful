---
title:                "Scaricare una pagina web"
html_title:           "C++: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

Ciao! Scopriamo insieme come scaricare una pagina web usando PHP.

## Che è e perché?

Scaricare una pagina web significa recuperare i dati della pagina da un URL specifico. I programmatori spesso lo fanno per analizzare il contenuto, expandere l'interoperabilità di app, o per l'indexing.

## Come fare:

Ecco un esempio di come scaricare una pagina web usando PHP:

```PHP
<?php
    $url = 'http://www.esempio.com';
    $pagina_web = file_get_contents($url);

    echo $pagina_web;
?>
```

In questo esempio, usiamo la funzione `file_get_contents` per scaricare i dati dell'URL dato, e poi li visualizziamo con `echo`.

## Approfondimento

- Storia: Il PHP (Preprocessor Hypertext) è nato nel 1994 come piccolo progetto open-source. La sua facilità d'uso lo ha presto reso uno strumento ideale per lo sviluppo web.
- Alternative: Esistono varie alternative per scaricare una pagina web. Alcuni esempi includono l'uso del comando cURL nella riga di comando, o l'uso di altre lingue come Python o JavaScript.
- Dettagli implementativi: `file_get_contents` lavora semplicemente facendo una richiesta GET all'URL specificato e ritornando la risposta. Non gestisce errori HTTP o altri problemi di rete, quindi potrebbe essere necessario gestire queste situazioni tu stesso in codice più complesso.

## Vedi anche

- PHP Manual, Funzione file_get_contents: https://www.php.net/manual/it/function.file-get-contents.php
- Tutorial su come usare cURL: https://www.computerhope.com/unix/curl.htm
- Python `requests` library, una possibile alternativa: https://docs.python-requests.org/en/master/ 

Ecco! Adesso dovresti avere una base di partenza per lavorare con il download delle pagine web in PHP. Buona codifica!