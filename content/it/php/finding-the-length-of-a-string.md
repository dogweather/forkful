---
title:                "Trovare la lunghezza di una stringa"
html_title:           "PHP: Trovare la lunghezza di una stringa"
simple_title:         "Trovare la lunghezza di una stringa"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché

Trovarе la lunghezza di una stringa è un'operazione comune nella programmazione e può essere utile per manipolare le stringhe in modi diversi. Conoscere il numero esatto di caratteri in una stringa può semplificare il lavoro di un sviluppatore e aiutare a scrivere codice più efficiente.

## Come Fare

Per trovare la lunghezza di una stringa in PHP è possibile utilizzare la funzione incorporata ```strlen()``` seguita dalla variabile contenente la stringa desiderata.

```
<?php
$stringa = "Questa è una stringa";
echo strlen($stringa); // output 20
```

In questo esempio, la funzione ```strlen()``` restituisce il numero totale di caratteri nella variabile ```$stringa```, inclusi gli spazi bianchi. È importante notare che la funzione conta ogni carattere, compresi gli spazi, i simboli e i caratteri speciali. Anche i caratteri di escape come ```\n``` e ```\t``` vengono considerati come un solo carattere.

## Approfondimento

PHP offre diverse alternative alla funzione ```strlen()``` per ottenere la lunghezza di una stringa. Ad esempio, è possibile utilizzare la funzione ```mb_strlen()``` per ottenere la lunghezza di una stringa mutlibyte, che gestisce correttamente i caratteri speciali di diverse lingue.

Inoltre, è importante ricordare che PHP tratta le stringhe come array di caratteri, quindi è possibile accedere ai singoli caratteri utilizzando gli indici dell'array e quindi utilizzare la funzione ```count()``` per ottenere la lunghezza totale della stringa.

```
<?php
$stringa = "Stringa";
echo count($stringa); // output 7
```

## Vedi Anche

- [Documentazione PHP su strlen()](https://www.php.net/manual/it/function.strlen.php)
- [Funzioni incorporate di PHP per le stringhe](https://www.php.net/manual/it/ref.strings.php)