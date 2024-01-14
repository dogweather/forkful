---
title:                "PHP: Convertire una stringa in minuscolo"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

Esistono molte ragioni per voler convertire una stringa in minuscolo durante la programmazione. Ad esempio, può essere utile per confrontare le stringhe senza dover considerare la differenza tra maiuscole e minuscole. Inoltre, molte funzioni di elaborazione dati richiedono input in minuscolo, quindi la conversione è spesso necessaria per poterle utilizzare.

## Come fare

La buona notizia è che in PHP esiste una funzione apposita per convertire una stringa in caratteri minuscoli: `strtolower()`. Basta passare la stringa come argomento della funzione e verrà restituita una nuova stringa tutta in minuscolo. Vediamo un esempio:

```PHP
$frase = "Questo e' il MIO testo";
$frase_minuscola = strtolower($frase);
echo $frase_minuscola;
```

Questo codice stampa: `questo e' il mio testo`. Possiamo notare che tutti i caratteri sono stati correttamente convertiti in minuscolo.

Vi sono anche altre funzioni che possono essere utili in alcuni casi specifici, come ad esempio `mb_strtolower()` per gestire correttamente caratteri multibyte.

## Approfondimento

Durante la conversione di una stringa in minuscolo, possono verificarsi alcune situazioni interessanti. Ad esempio, molte persone si chiedono come viene gestita la conversione dei caratteri accentati. In realtà, dipende dalla codifica dei caratteri utilizzata. Con codifiche come UTF-8, la conversione dei caratteri accentati in minuscolo è generalmente corretta. Tuttavia, con codifiche più limitate, potrebbero verificarsi errori o la mancata conversione dei caratteri accentati.

È anche importante tenere presente che, se si vuole utilizzare la stringa convertita in minuscolo per confrontarla con altre stringhe, è necessario utilizzare `mb_strtolower()` per gestire correttamente i caratteri multibyte.

In generale, è buona pratica convertire le stringhe in minuscolo prima di eseguire eventuali operazioni su di esse, in modo da evitare problemi di confronto e da mantenere una coerenza nei dati.

## Vedi anche

- Documentazione ufficiale di PHP sulla funzione `strtolower()`: https://www.php.net/manual/en/function.strtolower.php
- Guida all'utilizzo di codifiche di caratteri multibyte: https://www.php.net/manual/en/book.mbstring.php
- Esempi di conversione di stringhe in minuscolo in diversi contesti: https://stackoverflow.com/questions/4558109/convert-string-to-lowercase-in-php-based-on-locale