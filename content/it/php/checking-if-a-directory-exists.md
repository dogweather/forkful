---
title:    "PHP: Verifica dell'esistenza di una directory"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Perché

In questo post parleremo di come controllare se una directory esiste utilizzando PHP. Questa è una funzionalità molto utile per molti programmatori, in quanto permette di gestire meglio le directory e i file all'interno del proprio progetto.

## Come fare

Per controllare se una directory esiste in PHP, possiamo utilizzare la funzione `is_dir()`. Questa funzione restituisce un valore booleano (true o false) a seconda che la directory passata come parametro esista o meno.

```PHP
$directory = "/path/to/directory";

if (is_dir($directory)) {
  echo "La directory esiste.";
} else {
  echo "La directory non esiste.";
}
```

L'esempio sopra stamperà "La directory esiste." se la directory specificata esiste, altrimenti stamperà "La directory non esiste.".

## Approfondimento

Oltre alla funzione `is_dir()`, esistono altre funzioni utili per la gestione delle directory in PHP. Ad esempio, la funzione `mkdir()` permette di creare una nuova directory sul server. È inoltre possibile utilizzare la funzione `opendir()` per aprire una directory e visualizzare il suo contenuto.

Se si vuole controllare se una directory è scrivibile (cioè se è possibile creare o modificare i file al suo interno), si può utilizzare la funzione `is_writable()`. Questa restituisce un valore booleano a seconda che la directory sia scrivibile o meno.

È importante ricordare di gestire adeguatamente gli errori in caso di fallimento di una delle operazioni sopra descritte. Ad esempio, se la directory che si sta tentando di creare già esiste, la funzione `mkdir()` restituirà un errore.

## Vedi anche

- [Documentazione PHP: is_dir()](https://www.php.net/manual/en/function.is-dir.php)
- [Documentazione PHP: mkdir()](https://www.php.net/manual/en/function.mkdir.php)
- [Documentazione PHP: opendir()](https://www.php.net/manual/en/function.opendir.php)
- [Documentazione PHP: is_writable()](https://www.php.net/manual/en/function.is-writable.php)