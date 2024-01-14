---
title:    "PHP: Estrazione di sottostringhe"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Perché

Se stai cercando un modo efficiente per manipolare le stringhe in PHP, allora l'estrazione di sottostringhe potrebbe essere una tecnica utile da imparare. Questo ti permetterà di estrarre solo le parti delle stringhe che ti sono utili, risparmiando tempo e risorse nella codifica.

## Come fare

L'estrazione delle sottostringhe in PHP è molto semplice grazie alla funzione `substr()`. Ecco un esempio di come usarla per estrarre una sottostringa di 5 caratteri a partire dal quindicesimo carattere da una stringa:

```
PHP $stringa = "Benvenuti a PHP!";
$sottostringa = substr($stringa, 15, 5);
echo $sottostringa;
```
**Output:** `PHP!`

Puoi anche usare l'operatore di slicing `substr()` per estrarre una parte di una stringa basandoti su un determinato carattere. Ad esempio, puoi estrarre la parte della stringa che si trova prima dell'operatore '@' in un indirizzo email:

```
PHP $email = "esempio@email.com";
$username = substr($email, 0, strpos($email, "@"));
echo $username;
```
**Output:** `esempio`

## Approfondimento

Oltre alla funzione `substr()`, PHP offre anche altre funzioni utili per manipolare le stringhe. Ad esempio, `str_replace()` ti permette di sostituire parti di una stringa con un'altra. Puoi anche avvalerti di `explode()` per suddividere una stringa in un array basandoti su un carattere delimitatore.

Ricorda che quando si lavora con le stringhe in PHP, è importante gestire correttamente gli spazi vuoti e le lettere maiuscole/minuscole. Inoltre, puoi combinare diverse funzioni per ottenere risultati più complessi. Non avere paura di fare esperimenti per trovare il metodo migliore per le tue esigenze.

## Vedi anche

- [Documentazione ufficiale di PHP per la funzione substr()](https://www.php.net/manual/en/function.substr.php)
- [Tutorial su come manipolare le stringhe in PHP](https://www.tutorialspoint.com/php/php_string_functions.htm)
- [Esempi pratici di uso delle funzioni per stringhe in PHP](https://www.javatpoint.com/php-string-functions)