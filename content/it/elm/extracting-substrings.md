---
title:                "Estrazione di sottostringhe"
html_title:           "Elm: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

Molti di noi hanno avuto l'esperienza di dover estrarre una parte di una stringa di testo, come ad esempio un nome da un'indirizzo email o un'etichetta di un prodotto da un codice a barre. In Elm, esistono diverse funzioni utili per estrarre sottostringhe da una stringa più grande. In questo articolo, vedremo come utilizzare queste funzioni e perché potrebbero esserci utili nel tuo codice.

## Come Fare

Per estrarre una sottostringa in Elm, abbiamo due funzioni principali: `String.left` e `String.right`. Entrambe accettano due argomenti: la stringa di partenza e il numero di caratteri da estrarre.

Prendiamo per esempio una stringa contenente un indirizzo email:

```Elm
email: String
email = "mario.rossi@email.com"
```

Se vogliamo estrarre solo il nome utente, possiamo utilizzare la funzione `String.left` specificando il numero di caratteri da estrarre (nel nostro caso, 11):

```Elm
username: String
username = String.left email 11

-- Output: "mario.rossi"
```

Possiamo anche utilizzare la funzione `String.right` per estrarre la parte dell'email dopo il simbolo '@':

```Elm
domain: String
domain = String.right email 12

-- Output: "email.com"
```

Invece, se volessimo estrarre solo il nome della persona dalla stringa completa, possiamo utilizzare la funzione `String.slice`, che prende come argomenti la stringa di partenza, l'indice di inizio e l'indice di fine della sottostringa desiderata:

```Elm
name: String
name = String.slice email 6 11

-- Output: "rossi"
```

## Approfondimento

Oltre a queste tre funzioni, esistono altre opzioni per estrarre sottostringhe in Elm. Ad esempio, si può utilizzare la funzione `String.trim` per rimuovere eventuali spazi bianchi all'inizio e alla fine della stringa. Oppure si può utilizzare il metodo `String.split` per dividere una stringa in una lista di sottostringhe, utilizzando un carattere specificato come separatore.

È importante anche considerare la gestione degli errori quando si utilizzano queste funzioni. Se il numero di caratteri specificato è maggiore della lunghezza della stringa di partenza, si otterrà un errore di indice fuori range. Per evitare questo, è possibile utilizzare la funzione `String.length` per ottenere la lunghezza della stringa e fare un controllo prima di estrarre la sottostringa desiderata.

## Vedi Anche

- Documentazione ufficiale di Elm sulle funzioni di estrazione delle sottostringhe: https://package.elm-lang.org/packages/elm/core/latest/String
- Articolo su come utilizzare le funzioni di manipolazione delle stringhe in Elm: https://elmprogramming.com/string-manipulation-elm.html 
- Esempio di utilizzo delle funzioni di estrazione delle sottostringhe in un'applicazione Elm: https://github.com/elm-tutorial/example-substring-extraction