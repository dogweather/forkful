---
title:                "Cercare e sostituire testo"
html_title:           "PHP: Cercare e sostituire testo"
simple_title:         "Cercare e sostituire testo"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché

Se stai lavorando con testi in PHP, è possibile che a un certo punto tu abbia bisogno di effettuare una ricerca e sostituzione di determinati caratteri o parole. Questo può essere utile per risparmiare tempo e rendere più efficiente il tuo codice.

## Come Fare

Per effettuare una ricerca e sostituzione di testo in PHP, utilizzerai principalmente la funzione `str_replace()`. Questa funzione accetta tre parametri: il testo da cercare, il testo da sostituire e il testo in cui effettuare la ricerca e sostituzione.

Ecco un esempio pratico:

```PHP
$text = "Ciao Mondo! Benvenuti nel mio sito web!";
echo str_replace("Ciao", "Salve", $text);
```

Questo codice produrrà il seguente output:

```
Salve Mondo! Benvenuti nel mio sito web!
```

In questo caso, abbiamo cercato la parola "Ciao" nel testo e l'abbiamo sostituita con "Salve". Oltre a sostituire parole, è anche possibile utilizzare `str_replace()` per rimuovere determinati caratteri o stringhe da un testo.

Inoltre, è possibile utilizzare l'operatore `.` per concatenare più funzioni `str_replace()`, ad esempio:

```PHP
echo str_replace(",", "", str_replace(";", "!", "Ciao, come stai?"));
```

Questo produrrà il seguente output:

```
Ciao come stai!
```

## Approfondimento

Oltre alla funzione `str_replace()`, in PHP esiste anche la funzione `preg_replace()` che utilizza le espressioni regolari per effettuare una ricerca e sostituzione di testo. Questo può essere utile se hai bisogno di effettuare una sostituzione basata su un pattern specifico.

Ad esempio, se vuoi sostituire tutti i numeri in un testo con una stringa vuota, puoi utilizzare l'espressione regolare `/\d+/`:

```PHP
$text = "Ho vinto 100 euro al lotto!";
echo preg_replace("/\d+/", "", $text);
```

Questo produrrà il seguente output:

```
Ho vinto euro al lotto!
```

Come puoi vedere, la stringa "100" è stata sostituita con una stringa vuota, eliminando così la presenza dei numeri nel testo.

## Vedi Anche

- [Documentazione ufficiale di PHP sulle funzioni `str_replace()` e `preg_replace()`](https://www.php.net/manual/en/function.str-replace.php)
- [Guida alle espressioni regolari in PHP](https://www.php.net/manual/en/book.pcre.php)