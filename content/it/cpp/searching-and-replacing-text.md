---
title:                "C++: Ricerca e sostituzione del testo"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché

La sostituzione dei testi è un'operazione comune nella programmazione, sia per apportare modifiche rapide ai file che per automatizzare la modifica di un grande numero di testi. Con l'utilizzo dei giusti strumenti e tecniche, è possibile risparmiare tempo e ridurre gli errori umani nella sostituzione dei testi.

## Come fare

Per sostituire i testi in C++, esistono diverse opzioni a disposizione. Una di queste è l'utilizzo della funzione `replace()` che prende in input il testo originale, il testo da sostituire e il testo di sostituzione. Ad esempio:

```C++
string testo = "Ciao mondo!";
testo.replace(5, 6, "amici");
cout << testo << endl;
```
Output: Ciao amici!

In questo esempio, abbiamo sostituito le ultime sei lettere del testo originale con la parola "amici". L'utilizzo della funzione `replace()` è semplice ed efficace per sostituire parti specifiche di un testo.

Un'altra opzione è l'utilizzo del comando `sed` dalla riga di comando. Questo comando prevede un pattern di ricerca e un testo di sostituzione e può essere utilizzato per sostituire stringhe in un file di testo. Ad esempio:

```C++
system("sed -i 's/prodotto/prodotti/g' testo.txt");
```

In questo caso, il comando `sed` sostituisce tutte le occorrenze della parola "prodotto" con "prodotti" all'interno del file di testo "testo.txt".

## Approfondimento

La sostituzione dei testi può essere effettuata in modo molto più complesso e potente utilizzando espressioni regolari. Permettendo di specificare modelli di ricerca ancora più flessibili e precisi, le espressioni regolari sono uno strumento molto utile per la manipolazione dei testi.

Un'altra opzione è l'utilizzo di programmi di elaborazione dei testi come AWK o Perl, che consentono di scrivere script più avanzati per la sostituzione dei testi.

## Vedi anche

- [Funzione `replace()` in C++](https://www.cplusplus.com/reference/string/string/replace/)
- [Comando `sed` dalla riga di comando](https://www.gnu.org/software/sed/manual/sed.html)
- [Espressioni regolari in C++](https://www.geeksforgeeks.org/regular-expressions-in-c-c/)
- [AWK](https://www.gnu.org/software/gawk/manual/gawk.html)
- [Perl](https://www.perl.org/)