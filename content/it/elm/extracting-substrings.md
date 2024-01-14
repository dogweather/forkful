---
title:    "Elm: Estrazione di sottostringhe"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Perché
L'estrazione di sottostringhe è un'operazione comune quando si lavora con stringhe in Elm. Questo può essere utile per analizzare e manipolare grandi quantità di dati o per creare espressioni specifiche da utilizzare nelle nostre applicazioni.

## Come Fare
Per estrarre una sottostringa da una stringa esistente in Elm, possiamo utilizzare la funzione `String.slice` passandole tre parametri: la stringa originale, l'indice iniziale della sottostringa e l'indice finale.

```Elm
nomeCompleto = "Giorgio Rossi"
cognome = String.slice nomeCompleto 8 13

-- Output: "Rossi"
```

In questo esempio, abbiamo estratto la sottostringa del cognome dall'originale "Giorgio Rossi" utilizzando gli indici 8 e 13, che corrispondono rispettivamente alla posizione del primo e ultimo carattere. È importante notare che gli indici includono anche lo spazio, quindi l'ultimo carattere specificato non verrà incluso nella sottostringa finale.

Possiamo anche utilizzare segnaposto come `String.length` per ottenere l'indice finale senza doverlo calcolare manualmente. Ad esempio, per estrarre il nome dalla stessa stringa originale, possiamo usare il codice seguente:

```Elm
nome = String.slice nomeCompleto 0 (String.length nomeCompleto - 6)

-- Output: "Giorgio"
```

In questo caso, abbiamo utilizzato `String.length` per ottenere la lunghezza totale della stringa e quindi abbiamo sottratto 6 per escludere l'indice finale del cognome.

## Approfondimento
Oltre alla funzione `String.slice`, Elm offre anche altre funzioni utili per l'estrazione di sottostringhe come `String.left` e `String.right`, che ci consentono di estrarre rispettivamente una sottostringa dai primi o ultimi caratteri di una stringa.

Inoltre, possiamo utilizzare la funzione `String.split` per separare una stringa in diverse sottostringhe basate su un carattere specifico, ottenendo un elenco di sottostringhe come risultato.

## Vedi Anche
- Documentazione ufficiale di Elm su funzioni di stringhe: [https://elm-lang.org/docs/strings](https://elm-lang.org/docs/strings)
- Esempi di utilizzo di funzioni di stringhe in Elm: [https://github.com/giuseppeg/elm-strings](https://github.com/giuseppeg/elm-strings)