---
title:    "C++: Capitalizzare una stringa"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Capitalizzare una stringa è un'operazione comune nel mondo della programmazione, soprattutto quando si lavora con stringhe di testo. Ciò consente di rendere uniforme l'aspetto di una stringa, rendendola più leggibile e più facile da manipolare. Inoltre, in alcuni casi, è necessario capitalizzare una stringa per farla corrispondere correttamente con altre stringhe o per soddisfare specifici requisiti di un programma.

## Come Fare

Per capitalizzare una stringa in C++, è possibile utilizzare la funzione `toupper()` del linguaggio. Questa funzione converte ogni carattere della stringa in un carattere maiuscolo, mentre lascia invariati i caratteri non alfabetici. Di seguito è riportato un esempio di come utilizzare la funzione `toupper()` per capitalizzare una stringa:

```C++
#include <iostream> 
#include <cctype> 

using namespace std; 

int main() 
{ 
  // Input della stringa 
  string str = "ciao a tutti"; 
  
  cout << "Stringa non capitalizzata: " << str << endl; 
  
  // Loop per convertire ogni carattere in maiuscolo 
  for (char& ch : str) { 
    ch = toupper(ch); 
  } 
  
  cout << "Stringa capitalizzata: " << str << endl; 
  
  return 0; 
}
```

**Output:**

Stringa non capitalizzata: ciao a tutti

Stringa capitalizzata: CIAO A TUTTI

Nell'esempio sopra, abbiamo utilizzato un semplice loop per accedere a ogni carattere della stringa e applicare la funzione `toupper()` ad esso. Tuttavia, ci sono anche altre funzioni utili come `tolower()` per convertire una stringa in minuscolo o `toupper_l()` e `tolower_l()` per gestire la conversione dei caratteri in base alla lingua locale del sistema.

## Approfondimento

Una considerazione importante da tenere presente quando si capitalizza una stringa è il sistema operativo su cui verrà eseguito il programma. Infatti, il risultato della funzione `toupper()` può variare a seconda dell'ambiente in cui viene eseguito il programma. Ad esempio, su sistemi Windows, il carattere ß (es-zed tedesco) viene convertito in due caratteri, mentre su sistemi Linux viene convertito in un unico carattere.

Un altro aspetto importante è la gestione delle stringhe di lunghezza variabile, come ad esempio quelle create utilizzando il tipo di dato `std::string` in C++. In questo caso, è necessario utilizzare funzioni come `std::transform()` per gestire correttamente la capitalizzazione della stringa, evitando possibili errori di memoria o di buffer overflow.

Infine, è possibile personalizzare il comportamento della funzione `toupper()` creando una propria versione utilizzando la libreria `ctype.h` e definendo le regole di conversione dei caratteri desiderate.

## Vedi Anche

- [Documentazione ufficiale di toupper() in C++](https://www.cplusplus.com/reference/cctype/toupper/)
- [Altre funzioni di gestione delle stringhe in C++](https://www.geeksforgeeks.org/string-handling-standard-cpp/?ref=lbp)