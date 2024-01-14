---
title:    "PHP: Estrapolazione di sottostringhe"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché
Spesso durante la programmazione, è necessario lavorare con una stringa di testo più lunga estrarre solo una parte specifica. Questo è ciò che viene definito come estrazione di sottotringoli e può essere molto utile in diversi scenari di programmazione.

## Come fare
Per estrarre una sottostringa da una stringa più lunga, possiamo utilizzare la funzione `substr()` in PHP. Prendiamo ad esempio la seguente stringa:

```PHP
$stringa = "Questo è un esempio di una stringa.";
```

Se vogliamo estrarre solo la parola "esempio" da questa stringa, possiamo utilizzare la seguente istruzione:

```PHP
$sottostringa = substr($stringa, 11, 7);
```

Qui, stiamo specificando che vogliamo estrarre una sottostringa di 7 caratteri a partire dall'indice 11 della stringa originale. Ovviamente, questi valori possono essere variati in base alle nostre esigenze.

L'output di questo codice sarà:

```PHP
esempio
```

Possiamo anche specificare un solo parametro nella funzione `substr()` se vogliamo estrarre tutti i caratteri fino alla fine della stringa. Ad esempio:

```PHP
$altrosottostringa = substr($stringa, 27);
```

In questo caso, l'output sarà:

```PHP
una stringa.
```

Ci sono anche altre funzioni utili che possiamo utilizzare per estrarre sottotringoli, come ad esempio `strpos()`, `strrpos()`, `strstr()`, `strchr()`, etc. Consiglio di esplorare queste funzioni per avere una comprensione più approfondita.

## Approfondimenti
Oltre alla semplice estrazione di sottotringoli, ci sono alcune considerazioni importanti da tenere a mente. Ad esempio, dobbiamo fare attenzione agli indici quando utilizziamo le funzioni `substr()` e `strlen()` insieme. Inoltre, dobbiamo essere consapevoli di come le diverse funzioni gestiscono i caratteri speciali all'interno delle stringhe. Con una buona comprensione di questi aspetti, saremo in grado di utilizzare efficacemente le funzioni di estrazione di sottotringoli nella nostra programmazione.

## Vedi anche
- [Documentazione ufficiale di PHP su substr()](https://www.php.net/manual/en/function.substr.php)
- [Come estrarre una sottostringa in PHP](https://www.geeksforgeeks.org/how-to-extract-a-substring-from-a-string-in-php/)
- [Tutorial su stringhe in PHP](https://www.w3schools.com/php/php_strings_substrings.asp)