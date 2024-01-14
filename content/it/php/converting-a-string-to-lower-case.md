---
title:    "PHP: Convertire una stringa in minuscolo"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

Innanzitutto, perché vorresti convertire una stringa in minuscolo? Ci possono essere molte ragioni per farlo, ma una delle più comuni è per uniformare e standardizzare i dati. Ad esempio, se hai un elenco di nomi di utenti, potresti volerli convertire tutti in minuscolo per facilitare la ricerca e l'ordinamento.

## Come Fare

Per convertire una stringa in minuscolo in PHP, è possibile utilizzare la funzione `strtolower ()`. Ecco un esempio di codice che mostra come utilizzarla:

```PHP
$stringa = "Ciao Mondo!";
$stringa_minuscola = strtolower ($stringa);
echo $stringa_minuscola; // output: ciao mondo!
```

Come puoi vedere, la funzione `strtolower ()` accetta una stringa come argomento e la converte in minuscolo. È importante notare che questa funzione è "case-sensitive", quindi se la stringa contiene già lettere minuscole, non saranno convertite.

## Approfondimento

Se stai cercando di capire meglio come funziona la conversione di una stringa in minuscolo in PHP, potresti chiederti perché è necessario utilizzare una funzione apposita invece di semplicemente scrivere il testo in minuscolo manualmente.

La risposta è che la funzione `strtolower ()` tiene conto delle regole di localizzazione e linguaggio del sistema operativo su cui viene eseguito il codice. Questo significa che la conversione in minuscolo varierà a seconda della lingua e della localizzazione del sistema. Ad esempio, la lettera "i" in maiuscolo potrebbe essere convertita in "I" o "ı" a seconda delle impostazioni del sistema.

Inoltre, la funzione `strtolower ()` è in grado di gestire correttamente caratteri speciali e accentati nel caso in cui questi dovessero presentarsi nella stringa.

## Vedi Anche

- [Funzione strtolower () su PHP.net](https://www.php.net/manual/it/function.strtolower.php)
- [Cosa è la localizzazione in informatica - Wikipedia](https://it.wikipedia.org/wiki/Localizzazione_in_informatica)
- [Come gestire i caratteri speciali in PHP - Stack Overflow](https://stackoverflow.com/questions/5018914/how-to-handle-special-characters-in-php)