---
title:                "Convertire una stringa in minuscolo"
html_title:           "PHP: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Cosa e perché?

La conversione di una stringa in caratteri minuscoli è un'operazione comune che i programmatori effettuano per rendere uniforme il contenuto dei dati. Ciò è particolarmente utile quando si lavora con stringhe che possono contenere una combinazione di lettere maiuscole e minuscole, come ad esempio i nomi di utenti o password. 

La maggior parte dei linguaggi di programmazione, compreso PHP, forniscono una funzione predefinita per convertire una stringa in caratteri minuscoli. Convertire una stringa in minuscolo rende più facile confrontare e manipolare i dati all'interno del codice.

## Come fare:

```PHP
$stringa = "Marco";
echo strtolower($stringa);
```
Output: marco

La funzione "strtolower" accetta una stringa come parametro e restituisce la stessa stringa ma con tutti i caratteri convertiti in minuscolo.

## Approfondimento:

La conversione dei caratteri in minuscolo è una pratica comune che risale ai primi giorni della tecnologia informatica. In passato, quando il supporto dei caratteri maiuscoli e minuscoli nei computer era limitato, la conversione in minuscolo era necessaria per garantire la compatibilità dei dati.

Oltre alla funzione "strtolower", PHP offre anche altre due funzioni per manipolare il caso dei caratteri: "strtoupper" per convertire le stringhe in maiuscolo e "ucfirst" per convertire il primo carattere della stringa in maiuscolo.

Inoltre, è possibile utilizzare l'operatore "=" per assegnare una stringa in minuscolo a una variabile già esistente. Ad esempio: ```$stringa = "Nome Utente"; $stringa = strtolower($stringa);```

## Vedi anche:

- [Funzioni stringa di PHP](https://www.php.net/manual/en/ref.strings.php)
- [Strtolower nella documentazione ufficiale di PHP](https://www.php.net/manual/en/function.strtolower.php)
- [Tutorial su stringhe in PHP](https://www.w3schools.com/php/php_string.asp)