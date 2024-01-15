---
title:                "Estrazione di sottostringhe"
html_title:           "C++: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché usare l'estrazione di sottostringhe?

Quando si lavora con stringhe di testo, può essere utile prendere solo una parte specifica di esse per analisi o manipolazione. L'estrazione di sottostringhe è un'operazione essenziale che consente di estrarre parti di stringhe in base a determinati criteri.

## Come farlo?

Una volta definita una stringa da cui estrarre la sottostringa desiderata, è possibile utilizzare la funzione `substr` per specificare l'indice di inizio e la lunghezza della sottostringa da estrarre. Ad esempio:

```C++
string str = "Ciao a tutti!";
string sub = str.substr(5,3); // estrae i caratteri dalla posizione 5 per una lunghezza di 3
cout << sub; // output: a t
```

In alternativa, è possibile utilizzare il metodo `find` per cercare la posizione di un determinato carattere o stringa all'interno della stringa principale e poi utilizzare `substr` per estrarre la sottostringa desiderata. Ad esempio:

```C++
string str = "Hello world!";
int pos = str.find("world"); // cerca la posizione della stringa "world"
string sub = str.substr(pos); // estrae la parte di stringa a partire dalla posizione trovata
cout << sub; // output: world!
```

## Approfondimento

La funzione `substr` accetta anche un secondo parametro opzionale che permette di ottenere una sottostringa di lunghezza variabile a partire dalla posizione specificata. Inoltre, esiste anche il metodo `erase` che consente di eliminare una sottostringa all'interno della stringa principale.

## Vedi anche

- Documentazione ufficiale su `substr`: https://www.cplusplus.com/reference/string/string/substr/
- Tutorial su come usare `substr` in C++: https://www.geeksforgeeks.org/substring-in-cpp/
- Approfondimenti sull'utilizzo dei metodi `find` e `erase`: https://www.tutorialspoint.com/cplusplus/cpp_strings.htm