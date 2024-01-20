---
title:                "Verifica se una directory esiste"
html_title:           "PHP: Verifica se una directory esiste"
simple_title:         "Verifica se una directory esiste"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Che Cos'è e Perché?

Verificare se una directory esiste può sembrare banale, ma è fondamentale per molti programmi PHP. Questo controllo prevenire errori derivanti da operazioni su una directory inesistente e facilita la gestione delle risorse del file system.

## Ecco Come:
Il modo più semplice per controllare se una directory esiste in PHP è utilizzare la funzione `is_dir()`. Ecco un esempio di utilizzo:

```PHP
<?php
  $direttorio = '/percorsodeltuofile';

  if (is_dir($direttorio)) {
    echo 'La directory esiste!';
  } else {
    echo 'La directory non esiste!';
  }
?>
```

Quando si esegue questo script, verrà stampato 'La directory esiste!' se la directory esiste o 'La directory non esiste!' se non esiste. Ricorda, PHP è case-sensitive, quindi i nomi delle directory dovranno essere esatti!

## Approfondimenti:
Il principio dell'esistenza della directory è in atto da quando i file system sono stati introdotti nei computer. In PHP, la funzione `is_dir()` è disponibile dalla versione 4.0.0, rendendola una delle funzioni più storiche del linguaggio.

Come alternativa, è possibile utilizzare le funzioni `file_exists()` o `opendir()`. La prima verifica l'esistenza di un file o una directory, mentre la seconda tenta di aprire una directory e restituisce FALSE se non riesce.

Per quanto riguarda l'implementazione, `is_dir()` chiama semplicemente la funzione di sistema `stat` sul sistema operativo. Questa funzione restituisce una struttura di dati che contiene tutte le informazioni sul file, inclusa la sua tipologia.