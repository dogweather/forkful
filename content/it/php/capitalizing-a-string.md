---
title:    "PHP: Maiuscolare una stringa."
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Perché

Capitalizzare una stringa è un'azione comune nella programmazione, soprattutto quando si lavora con dati da input degli utenti. Questo permette di uniformare il formato delle stringhe per facilitare la loro gestione e manipolazione.

## Come Fare

Per capitalizzare una stringa in PHP, possiamo utilizzare la funzione `strtoupper()`. Questa funzione accetta come parametro la stringa che vogliamo capitalizzare e restituisce una nuova stringa con tutti i caratteri maiuscoli.

```PHP
$text = "ciao, come stai?";
$text_upper = strtoupper($text);
echo $text_upper;
```

Questo codice produrrà il seguente output:

```
CIAO, COME STAI?
```

Anche la funzione `ucfirst()` può essere utilizzata per capitalizzare solo la prima lettera della stringa:

```PHP
$text = "ciao, come stai?";
$text_ucfirst = ucfirst($text);
echo $text_ucfirst;
```

Questo produrrà il seguente output:

```
Ciao, come stai?
```

## Approfondimento

Mentre la funzione `strtoupper()` è utile per capitalizzare l'intera stringa, è importante notare che non modifica la stringa originale, ma ne restituisce una nuova. Inoltre, è possibile utilizzare altre funzioni come `strtolower()` per trasformare la stringa in tutto minuscolo o `ucwords()` per capitalizzare ogni parola nella stringa.

Inoltre, è possibile specificare la lingua nella quale vogliamo capitalizzare la stringa utilizzando la funzione `setlocale()`:

```PHP
setlocale(LC_ALL,"ita_ITA");
$italian_text = "ciao a tutti";
$italian_text_upper = strtoupper($italian_text);
echo $italian_text_upper;
```

Questo produrrà il seguente output:

```
CIAO A TUTTI
```

## Vedi Anche

- Documentazione ufficiale PHP per la funzione `strtoupper()`: https://www.php.net/manual/en/function.strtoupper.php
- Articolo su Stack Overflow sulle diverse funzioni per capitalizzare una stringa in PHP: https://stackoverflow.com/questions/7704879/how-to-capitalize-the-first-letter-of-every-word-in-a-string-in-php
- Tutorial su come utilizzare la funzione `setlocale()`: https://www.tutorialspoint.com/how-to-use-setlocale-in-php