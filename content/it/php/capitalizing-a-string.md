---
title:                "PHP: Capitalizzazione di una stringa"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Capitalize è una funzione semplice ma potente in PHP che può essere utile in molte situazioni di programmazione. Ad esempio, può essere utilizzata per rendere più leggibile una stringa o per uniformare il formato delle parole all'interno di un database di dati.

## Come fare

Per capitalizzare una stringa in PHP, è necessario utilizzare la funzione `ucfirst()`. Questo metodo accetta una stringa come argomento e restituisce la stessa stringa con la prima lettera in maiuscolo. Ecco un esempio:

```PHP
$stringa = "ciao a tutti!";
$stringa = ucfirst($stringa);
echo $stringa;
```

L'output di questo codice sarà "Ciao a tutti!". Nota che la funzione `ucfirst()` solo la prima lettera della stringa, quindi se si vuole capitalizzare l'intera stringa, è necessario utilizzare la funzione `ucwords()`.

```PHP
$stringa = "ciao a tutti!";
$stringa = ucwords($stringa);
echo $stringa;
```

L'output sarà "Ciao A Tutti!".

## Deep Dive

Oltre alla funzione `ucfirst()` e `ucwords()`, PHP offre anche la funzione `strtoupper()` e `strtolower()` per trasformare una stringa intera in caratteri maiuscoli o minuscoli.

Ci sono anche alcune opzioni avanzate per capitalizzare una stringa, come utilizzare le espressioni regolari e la funzione `mb_convert_case()` per gestire correttamente i caratteri accentati.

Inoltre, è possibile utilizzare la funzione `setlocale()` per applicare la capitalizzazione in base alle regole di una lingua specifica. Per ulteriori informazioni su queste opzioni avanzate, consultare la documentazione ufficiale di PHP.

## Vedi anche

- [Documentazione ufficiale di PHP per la funzione ucfirst()](https://www.php.net/manual/en/function.ucfirst.php)
- [Documentazione ufficiale di PHP per la funzione ucwords()](https://www.php.net/manual/en/function.ucwords.php)
- [Documentazione ufficiale di PHP per la funzione strtoupper()](https://www.php.net/manual/en/function.strtoupper.php)
- [Documentazione ufficiale di PHP per la funzione strtolower()](https://www.php.net/manual/en/function.strtolower.php)
- [Documentazione ufficiale di PHP per la funzione mb_convert_case()](https://www.php.net/manual/en/function.mb-convert-case.php)
- [Documentazione ufficiale di PHP per la funzione setlocale()](https://www.php.net/manual/en/function.setlocale.php)