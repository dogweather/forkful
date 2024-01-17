---
title:                "Maiuscolare una stringa"
html_title:           "PHP: Maiuscolare una stringa"
simple_title:         "Maiuscolare una stringa"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

Capitalizzare una stringa significa convertire la prima lettera di ogni parola in maiuscolo. I programmatori spesso lo fanno per rendere più leggibili le stringhe di testo, in particolare quando si tratta di titoli o frasi importanti.

## Come fare:

Puoi capitalizzare una stringa utilizzando la funzione `ucwords()` di PHP. Vediamo un esempio:

```PHP
$stringa = "ciao a tutti gli amici";
echo ucwords($stringa);

// Output: Ciao A Tutti Gli Amici
```

Puoi anche capitalizzare una stringa utilizzando il metodo `ucfirst()` che converte solo la prima lettera in maiuscolo. Esempio:

```PHP
$stringa = "saluti dalla bella Italia";
echo ucfirst($stringa);

// Output: Saluti dalla bella Italia
```

## Approfondimento:

La pratica di capitalizzare le stringhe ha origine dalla stampa tipografica, dove le prime lettere delle parole erano generalmente più grandi e audaci rispetto al resto del testo. In alternativa, in alcuni casi può essere più appropriato utilizzare la funzione `strtoupper()` per convertire l'intera stringa in maiuscolo.

## Vedi anche:

- Documentazione PHP per la funzione `ucwords()`: https://www.php.net/manual/en/function.ucwords.php
- Documentazione PHP per la funzione `ucfirst()`: https://www.php.net/manual/en/function.ucfirst.php
- Documentazione PHP per la funzione `strtoupper()`: https://www.php.net/manual/en/function.strtoupper.php