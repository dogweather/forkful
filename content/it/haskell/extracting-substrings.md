---
title:                "Estrazione di sottostringhe"
html_title:           "Haskell: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

# Perché

Sei mai stato frustrato nel dover estrarre una parte di una stringa in un programma? Forse vuoi solo ottenere il nome di un file da un percorso completo o il giorno di una data. In ogni caso, l'estrazione di sottostringhe può semplificare notevolmente la tua vita di programmazione.

# Come Farlo

Fortunatamente, Haskell ha una sintassi semplice e potente per estrarre sottostringhe. Ecco un esempio di come estrarre una parte di una stringa utilizzando la funzione `take`:

```Haskell
take 6 "Haskell è fantastico!"
```

Ciò restituirà la sottostringa "Haskell" dalla stringa data come input. La funzione `take` prende due argomenti: il numero di caratteri da estrarre e la stringa di input. Puoi persino estrarre una sottostringa da una stringa più grande utilizzando la funzione `take` in modo ricorsivo:

```Haskell
takeSubstr :: Int -> Int -> String -> String
takeSubstr start end str = take (end-start) (drop start str)
```

In questo esempio, forniamo tre argomenti alla funzione `takeSubstr`: l'indice di inizio, l'indice di fine e la stringa di input. Quindi, utilizziamo le funzioni `take` e `drop` per estrarre la sottostringa desiderata. Ad esempio, possiamo utilizzare questa funzione per ottenere il giorno da una data nel formato "dd/mm/yyyy":

```Haskell
takeSubstr 0 2 "17/05/2021"
```

Questo ci restituirà la sottostringa "17" che rappresenta il giorno.

# Approfondimento

Adesso che abbiamo visto come estrarre sottostringhe, vale la pena notare che è possibile utilizzare una notazione speciale per rendere il codice più leggibile. Ad esempio, invece di scrivere `take 6 "Haskell è fantastico!"`, possiamo utilizzare la notazione `take 6 $ "Haskell è fantastico!"`. Invece di passare due argomenti separati alla funzione `takeSubstr`, possiamo utilizzare la notazione `takeSubstr 0 2 "17/05/2021"`.

Inoltre, puoi utilizzare la funzione `dropWhile` per estrarre una sottostringa a partire dall'inizio della stringa e continuando a ignorare i caratteri fintanto che non viene soddisfatta una determinata condizione. Per esempio, se volessimo ottenere la parola "è" dalla stringa "Haskell è fantastico!", potremmo farlo in questo modo:

```Haskell
takeWhile (/= ' ') $ drop 8 "Haskell è fantastico!"
```

Ciò restituirà la sottostringa "è". Nota come utilizziamo la funzione "takesWhile" e il predicato `(/= ' ')` per ignorare tutti i caratteri fino a quando non raggiungiamo uno spazio bianco.

# Vedi Anche

- [Documentazione ufficiale di Haskell](https://www.haskell.org/documentation/)
- [Tutorial introduttivo di Haskell](https://learnxinyminutes.com/docs/it-it/haskell-it/)
- [13 cose che non capivo su Haskell fino a due giorni fa](https://medium.com/@jakubarnold/13-cose-che-non-capivo-su-haskell-fino-a-due-giorni-fa-f3c9b44a15c)