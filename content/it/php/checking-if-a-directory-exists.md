---
title:    "PHP: Verificare l'esistenza di una directory"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché

Spesso, mentre programmi in PHP, potresti aver bisogno di verificare se una directory esiste prima di eseguire alcune operazioni su di essa. Questo può essere utile per evitare errori e garantire che il tuo codice funzioni correttamente.

## Come Fare

Per verificare se una directory esiste in PHP, puoi utilizzare la funzione `is_dir()`. Questa funzione prenderà un percorso di directory come input e restituirà un valore booleano `true` se la directory esiste o `false` se non esiste.

```
<?php 
// Verifica se la directory "immagini" esiste
if(is_dir("immagini")){
  echo "La directory esiste";
} else{
  echo "La directory non esiste";
}
?>
```
Output:
```
La directory esiste
```

Puoi anche utilizzare la funzione `file_exists()`, che accetta anche file come input. Questa funzione restituirà `true` se il file o la directory esiste e `false` se non esiste.

```
<?php
// Verifica se il file "logo.jpg" esiste
if(file_exists("logo.jpg")){
  echo "Il file esiste";
} else{
  echo "Il file non esiste";
}
?>
```
Output:
```
Il file non esiste
```

## Approfondimento

È importante notare che queste funzioni restituiranno anche `true` se il file o la directory sono vuoti. Inoltre, se il percorso fornito non è valido, entrambe le funzioni restituiranno `false`. Quindi, è una buona pratica verificare anche se la directory o il file ha contenuto utilizzando la funzione `scandir()`.

Inoltre, puoi utilizzare la funzione `is_readable()` per verificare se la directory è leggibile e `is_writable()` per verificare se la directory è scrivibile.

## Vedi Anche

- [La documentazione di PHP su is_dir()](https://www.php.net/manual/en/function.is-dir.php)
- [La documentazione di PHP su file_exists()](https://www.php.net/manual/en/function.file-exists.php)
- [La documentazione di PHP su scandir()](https://www.php.net/manual/en/function.scandir.php)
- [La documentazione di PHP su is_readable()](https://www.php.net/manual/en/function.is-readable.php)
- [La documentazione di PHP su is_writable()](https://www.php.net/manual/en/function.is-writable.php)