---
title:                "Capitalizzazione di una stringa"
html_title:           "PHP: Capitalizzazione di una stringa"
simple_title:         "Capitalizzazione di una stringa"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Capitalize è una operazione comune nella programmazione che permette di rendere la prima lettera di una stringa maiuscola. Questo può essere utile per correggere eventuali errori di formattazione o per rendere più leggibile una stringa di testo.

## Come

```PHP 
// Utilizziamo la funzione ucfirst() per capitalizzare la prima lettera di una stringa
$stringa = "ciao mondo";
echo ucfirst($stringa);
```

L'output di questo codice sarà "Ciao mondo". Possiamo anche utilizzare la funzione ucwords() per capitalizzare la prima lettera di ogni parola in una stringa.

```PHP
// Utilizziamo la funzione ucwords() per capitalizzare la prima lettera di ogni parola in una stringa
$stringa = "ciao mondo";
echo ucwords($stringa);
```

L'output di questo codice sarà "Ciao Mondo". Inoltre, PHP offre anche la funzione strtoupper() che permette di rendere tutte le lettere di una stringa maiuscole.

```PHP
// Utilizziamo la funzione strtoupper() per rendere maiuscole tutte le lettere di una stringa
$stringa = "ciao mondo";
echo strtoupper($stringa);
```

L'output di questo codice sarà "CIAO MONDO".

## Deep Dive

In PHP, le stringhe sono immutabili, il che significa che non possiamo modificarle direttamente una volta che sono state create. Quando utilizziamo una funzione per capitalizzare una stringa, in realtà stiamo creando una nuova stringa con le modifiche desiderate. Questo può essere un aspetto importante da considerare durante la scrittura del nostro codice.

Inoltre, è importante notare che le funzioni di capitalizzazione in PHP seguono le regole di localizzazione, il che significa che il risultato finale può variare in base alla lingua impostata nel server. Ad esempio, in italiano la funzione ucfirst() renderà "café" come "Café", mentre in inglese diventerà "Café".

## Vedi anche

- [Documentazione ufficiale di ucfirst()](https://www.php.net/manual/en/function.ucfirst.php)
- [Documentazione ufficiale di ucwords()](https://www.php.net/manual/en/function.ucwords.php)
- [Documentazione ufficiale di strtoupper()](https://www.php.net/manual/en/function.strtoupper.php)