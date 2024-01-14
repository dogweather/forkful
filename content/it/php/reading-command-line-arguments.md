---
title:                "PHP: Lettura degli argomenti della riga di comando"
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

Gli argomenti della riga di comando sono uno strumento importante per un programmatore PHP, in quanto consentono di passare informazioni al programma durante l'esecuzione. Imparare come leggere gli argomenti della riga di comando può aiutare a creare applicazioni più flessibili e interattive.

## Come fare

Per leggere gli argomenti della riga di comando in PHP, è necessario utilizzare la funzione `getopt()`. Questa funzione accetta due argomenti: una stringa contenente le opzioni accettate e un array contenente i valori degli argomenti passati. Vediamo un esempio:

```
<?php
// Esempio di utilizzo della funzione getopt()
$options = getopt("a:b:");
var_dump($options);
```

Se questo programma viene eseguito da riga di comando con l'opzione "-a valore1 -b valore2", verrà restituita la seguente informazione:

```
array(2) {
  ["a"]=>
  string(7) "valore1"
  ["b"]=>
  string(7) "valore2"
}
```

È possibile accedere ai valori degli argomenti utilizzando la loro chiave nell'array restituito dalla funzione. Se ad esempio vogliamo ottenere il valore dell'opzione "a" nel nostro esempio, possiamo utilizzare `$options["a"]`.

## Deep Dive

Vediamo ora qualche esempio più complesso per capire meglio come funzionano gli argomenti della riga di comando in PHP. Innanzitutto, la stringa contenente le opzioni accettate può essere modificata in modo da specificare quali opzioni sono obbligatorie o impostare valori di default. Possiamo anche utilizzare un carattere "!" per indicare che l'opzione non richiede un valore.

```
<?php
// Esempio di utilizzo della funzione getopt() con opzioni obbligatorie
$options = getopt("a:!b:c:", ["a:", "b:", "c:"]);
var_dump($options);
```

Se ad esempio eseguiamo questo programma con le opzioni "-a valore1 -c valore2", avremo come risultato:

```
array(2) {
  ["a"]=>
  string(7) "valore1"
  ["c"]=>
  string(7) "valore2"
}
```

In caso di opzioni obbligatorie mancanti, la funzione `getopt()` restituirà un errore.

## Vedi anche

- [Documentazione ufficiale di PHP su getopt()](https://www.php.net/manual/en/function.getopt.php)
- [Tutorial su come leggere argomenti della riga di comando in PHP](https://www.tutorialspoint.com/php/php_command_line.htm)
- [Esempi pratici di utilizzo di getopt()](https://php.developreference.com/article/22298628/PHP+How+to+use+getopt%28%29)