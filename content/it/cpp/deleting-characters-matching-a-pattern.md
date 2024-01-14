---
title:    "C++: Cancellazione di caratteri corrispondenti a uno schema"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

Spesso, durante lo sviluppo di un programma, può essere necessario eliminare caratteri specifici da una stringa. Ciò può essere utile, ad esempio, per la pulizia dei dati o per soddisfare determinati requisiti del codice.

## Come fare

Per eliminare caratteri corrispondenti a un determinato pattern all'interno di una stringa, possiamo utilizzare la funzione `erase()` in combinazione con il metodo `erase_if()` disponibile nella libreria `<algorithm>`. Ecco un esempio di codice in C++:

```C++
std::string str = "Ciao, come stai?";
// elimina tutte le lettere maiuscole dalla stringa
str.erase(std::remove_if(str.begin(), str.end(), ::isupper), str.end());
// output: "iao, come stai?"
```

In questo esempio, la funzione `erase()` viene utilizzata per eliminare ogni carattere della stringa che corrisponde alla condizione definita dalla funzione `remove_if()`. Quest'ultima, a sua volta, utilizza il parametro `::isupper` per verificare se il carattere è maiuscolo o meno.

## Approfondimento

La funzione `erase_if()` ci permette di utilizzare qualsiasi tipo di condizione per eliminare i caratteri desiderati. Ad esempio, possiamo utilizzare una lambda expression per definire una condizione personalizzata:

```C++
std::string str = "Lorem ipsum dolor sit amet";
// elimina tutte le vocali dalla stringa
str.erase(std::remove_if(str.begin(), str.end(), [](char c) { return c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u'; }), str.end());
// output: "Lrm psm dlr st mt"
```

Nel codice sopra, la lambda expression controlla se il carattere corrente è una vocale e se lo è, viene eliminato dalla stringa.

Inoltre, è possibile combinare più funzioni della libreria `<algorithm>` per ottenere risultati più complessi. Ad esempio, possiamo eliminare tutti i caratteri che non sono lettere o numeri dalla stringa utilizzando `remove_if()` in combinazione con la funzione `isalnum()`:

```C++
std::string str = "QWERTY#123";
// elimina tutti i caratteri non alfanumerici dalla stringa
str.erase(std::remove_if(str.begin(), str.end(), [](char c) { return !std::isalnum(c); }), str.end());
// output: "QWERTY123"
```

## Vedi anche

- [Funzione `erase()` in C++](https://www.w3schools.com/cpp/cpp_strings_erase.asp)
- [Confronto tra `remove_if()` e `erase_if()`](https://stackoverflow.com/questions/31540361/what-is-the-difference-between-stdremove-if-and-stderase-if)
- [Introduzione alle lambda expressions in C++](https://www.freecodecamp.org/news/how-to-use-lambda-expressions-in-cpp/)