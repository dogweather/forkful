---
title:                "PHP: Estrazione di sottostringhe"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

Estrazione di sub-stringhe è un concetto fondamentale nella programmazione PHP. Ci permette di ottenere parti specifiche di una stringa, come ad esempio una parola o una frase specifica. Questa tecnica è estremamente utile e viene utilizzata in diverse situazioni, come nella gestione di dati di form o nella manipolazione di url.

## Come fare

Per estrarre una sub-stringa in PHP, utilizziamo la funzione `substr()`, che prende tre argomenti: una stringa di origine, l'indice di inizio e la lunghezza della sub-stringa desiderata. Ad esempio, per estrarre la parola "programmazione" dalla stringa "Questo è un corso di programmazione", useremmo il seguente codice:

```PHP
$stringa = "Questo è un corso di programmazione";
$parola = substr($stringa, 16, 13);
echo $parola; // output: programmazione
```

Se invece vogliamo ottenere la prima parola della stringa, possiamo utilizzare la funzione `explode()`, che divide la stringa in base a un delimitatore specificato e restituisce un array. Ad esempio, per ottenere la parola "Questo" dalla stringa precedente, useremmo il seguente codice:

```PHP
$stringa = "Questo è un corso di programmazione";
$prima_parola = explode(" ", $stringa)[0];
echo $prima_parola; // output: Questo
```

## Approfondimento

Esistono altri metodi per estrarre sub-stringhe, come ad esempio utilizzare le funzioni `strpos()` e `strrpos()` per ottenere l'indice iniziale della sub-stringa desiderata e poi utilizzare la funzione `substr()` per estrarla. Inoltre, è anche possibile utilizzare le espressioni regolari per estrarre sub-stringhe in base a un pattern specifico.

È importante ricordare che l'indice di una stringa inizia sempre da 0, quindi la prima lettera di una stringa ha indice 0, la seconda ha indice 1, e così via.

## Vedi anche

- [Documentazione ufficiale di PHP su substr()](https://www.php.net/manual/en/function.substr.php)
- [Esempi pratici di estrazione di sub-stringhe in PHP](https://www.tutorialspoint.com/php/php_string_substr.htm)
- [Approfondimento sulle espressioni regolari in PHP](https://www.php.net/manual/en/function.preg-match.php)