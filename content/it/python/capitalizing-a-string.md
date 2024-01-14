---
title:                "Python: Capitalizzazione di una stringa"
simple_title:         "Capitalizzazione di una stringa"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché
La capitalizzazione di una stringa è un'operazione comune nella programmazione Python che permette di modificare la formattazione di una stringa, rendendo la prima lettera maiuscola e il resto in minuscolo. Questo può essere utile per rendere più leggibili i testi o per adattarli a determinati standard di formattazione.

## Come fare
Per capitalizzare una stringa in Python, è possibile utilizzare il metodo `capitalize()` che fa parte della classe `str`. Ad esempio:

```
nome = "marco"
nome_capitalizzato = nome.capitalize()
print(nome_capitalizzato)
```
Output: Marco

Possiamo anche applicare la capitalizzazione solo alla prima lettera di ogni parola in una stringa utilizzando il metodo `title()`. Ad esempio:

```
testo = "questo è un esempio di stringa da capitalizzare"
testo_capitalizzato = testo.title()
print(testo_capitalizzato)
```
Output: Questo È Un Esempio Di Stringa Da Capitalizzare

Per applicare la capitalizzazione ad una stringa immessa dall'utente, possiamo utilizzare la funzione `input()` per richiedere l'input all'utente e poi applicare il metodo `capitalize()` o `title()`. Ad esempio:

```
nome = input("Inserisci il tuo nome: ")
nome_capitalizzato = nome.capitalize()
print("Ciao " + nome_capitalizzato + ", benvenuto!")
```

## Approfondimento
Durante la capitalizzazione di una stringa, Python non modifica l'originale, ma ne crea una copia con la formattazione corretta. Inoltre, se la stringa originale ha già una prima lettera maiuscola, il metodo `capitalize()` non la modificherà.

Un altro metodo utile per la formattazione di una stringa è `swapcase()` che si occupa di invertire la capitalizzazione delle lettere all'interno della stringa, rendendo maiuscole le minuscole e viceversa. Ad esempio:

```
testo = "ProVa Ad AppLiCare La CaPitalizZazione"
testo_modificato = testo.swapcase()
print(testo_modificato)
```
Output: pROvA aD aPPlIcARE lA cApITALIZZAZIONE

## Vedi anche
- [Documentazione Python su stringhe](https://docs.python.org/3/library/stdtypes.html#string-methods)
- [Capitalizzazione di una stringa in Python](https://www.programiz.com/python-programming/methods/string/capitalize)