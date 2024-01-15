---
title:                "Verificare se una directory esiste"
html_title:           "PHP: Verificare se una directory esiste"
simple_title:         "Verificare se una directory esiste"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché

Controllare se una directory esiste è fondamentale quando si lavora con file e cartelle in un progetto PHP. Questo consente di garantire che il codice funzioni correttamente in tutte le situazioni e di gestire eventuali errori in modo efficiente.

## Come fare

In PHP, esistono diverse funzioni che possono essere utilizzate per verificare l'esistenza di una directory. In questa sezione, vedremo tre metodi comuni per eseguire questa operazione.

### Utilizzando realpath()

Il primo metodo consiste nell'utilizzare la funzione `realpath()` che restituisce il percorso assoluto di una directory o di un file. Se la directory esiste, la funzione restituirà il suo percorso assoluto, altrimenti restituirà `false`.

```PHP
$directory = "/path/to/directory";

if (realpath($directory)) {
  echo "La directory esiste.";
} else {
  echo "La directory non esiste.";
}
```

#### Esempio di output

Se la directory esiste, l'output sarà:

```
La directory esiste.
```

In caso contrario, l'output sarà:

```
La directory non esiste.
```

### Utilizzando file_exists()

Un altro modo per verificare l'esistenza di una directory è utilizzare la funzione `file_exists()`, che controlla se un file o una directory esiste.

```PHP
$directory = "/path/to/directory";

if (file_exists($directory)) {
  echo "La directory esiste.";
} else {
  echo "La directory non esiste.";
}
```

#### Esempio di output

Se la directory esiste, l'output sarà:

```
La directory esiste.
```

In caso contrario, l'output sarà:

```
La directory non esiste.
```

### Utilizzando is_dir()

Infine, la funzione `is_dir()` può essere utilizzata per controllare se un percorso corrisponde a una directory. La funzione restituirà `true` se il percorso è una directory, altrimenti restituirà `false`.

```PHP
$directory = "/path/to/directory";

if (is_dir($directory)) {
  echo "La directory esiste.";
} else {
  echo "La directory non esiste.";
}
```

#### Esempio di output

Se la directory esiste, l'output sarà:

```
La directory esiste.
```

In caso contrario, l'output sarà:

```
La directory non esiste.
```

## Approfondimento

È importante notare che quando si utilizza `is_dir()`, il percorso deve corrispondere a una directory, non a un file. Se il percorso corrisponde a un file, la funzione restituirà sempre `false`. Inoltre, tutte e tre le funzioni descritte sopra possono anche essere utilizzate per verificare l'esistenza di un file, non solo di una directory.

## Vedi anche

- [Documentazione ufficiale di PHP: realpath()](https://www.php.net/manual/it/function.realpath.php)
- [Documentazione ufficiale di PHP: file_exists()](https://www.php.net/manual/it/function.file-exists.php)
- [Documentazione ufficiale di PHP: is_dir()](https://www.php.net/manual/it/function.is-dir.php)