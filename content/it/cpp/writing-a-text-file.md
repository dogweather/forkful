---
title:    "C++: Scrivere un file di testo"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Perché

Scrivere un file di testo può sembrare una semplice operazione, ma può essere incredibilmente utile per diverse ragioni. Innanzitutto, ti permette di salvare informazioni importanti in un formato facilmente leggibile e modificabile. Inoltre, può essere un utile strumento per la creazione di programmi e script più complessi.

## Come fare

Per scrivere un file di testo in C++, è necessario utilizzare la classe ```ofstream```. Questa classe ci permette di aprire un file in modalità scrittura e di scrivere all'interno del file utilizzando l'operatore di inserimento ```<<```.

```
#include <iostream>
#include <fstream> // necessario per utilizzare ofstream
using namespace std;

int main() {
   // crea un oggetto ofstream e apre il file "esempio.txt"
   ofstream file("esempio.txt");

   // scrive una stringa all'interno del file
   file << "Questo è un esempio di file di testo scritto in C++!" << endl;

   // chiude il file
   file.close();

   return 0;
}
```

Una volta che il file è stato chiuso, è possibile aprirlo manualmente per vedere il risultato. Il file dovrebbe contenere la stringa che abbiamo inserito nel nostro programma.

## Approfondimento

Oltre all'utilizzo della classe ```ofstream```, è possibile creare file di testo utilizzando anche altre funzioni e librerie, come ad esempio ```fopen``` e ```fprintf``` della libreria ```<cstdio>```. Inoltre, è importante tenere in considerazione il sistema operativo su cui il programma sarà eseguito, poiché ci possono essere differenze nel formato di fine riga utilizzato (ad esempio, ```\n``` su Linux e ```\r\n``` su Windows).

## Vedi anche

- [Documentazione ufficiale di C++ per ofstream](https://en.cppreference.com/w/cpp/io/basic_ofstream)
- [Tutorial su come scrivere file di testo in C++](https://www.learncpp.com/cpp-tutorial/186-basic-file-io/)
- [Esempi di codice su GitHub per scrivere file di testo in C++](https://github.com/search?q=cpp+write+text+file&type=Repositories)