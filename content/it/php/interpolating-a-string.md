---
title:                "Interpolazione di una stringa"
html_title:           "Clojure: Interpolazione di una stringa"
simple_title:         "Interpolazione di una stringa"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Cosa e Perché?

L'interpolazione di stringhe è un modo semplice e veloce per inserire il valore di una variabile all'interno di una stringa. Questo permette di ottenere un output più personalizzato e dinamico.

## Come fare:

In PHP, puoi interpolare una stringa utilizzando le doppie virgolette. Ad esempio:

```PHP
$nome = "Mario";
echo "Ciao, $nome!";
```

Il tuo output sarà:

```PHP
Ciao, Mario!
```

Ora, se hai bisogno di concatenare delle stringhe con del testo costante, non avrai bisogno di usare il punto. Ad esempio:

```PHP
$nome = "Mario";
echo "Il tuo nome è $nome, vero?";
```

Il tuo output sarà:

```PHP
Il tuo nome è Mario, vero?
```

## Approfondimento:

L'interpolazione delle stringhe è possibile da PHP 4 in poi. Questa funzionalità offre un'alternativa più leggibile e veloce alla concatenazione di stringhe, scongiurando il rischio di errori di sintassi dovuti all'uso eccessivo di punti e virgolette.

Un'alternativa all'interpolazione è l'uso della funzione sprintf(), ma l'interpolazione di stringhe è generalmente considerata più semplice e leggibile.

Per quanto riguarda i dettagli di implementazione, è importante notare che l'interpolazione di stringhe può essere usata con variabili di base, ma non con espressioni complesse. Ad esempio, `echo "Il risultato è {$num * $num}";` darà errore.


## Per saperne di più:

Per ulteriori informazioni sull'interpolazione delle stringhe in PHP, consulta queste risorse:

- [Uso di variabili all'interno di stringhe in PHP](https://stackoverrun.com/it/q/8723283)