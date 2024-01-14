---
title:                "C++: Lettura di un file di testo"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché leggere un file di testo in C++

Lettura di file di testo è un compito comune nella programmazione in C++. Potresti voler leggere un file per recuperare dati, come un elenco di nomi o numeri, oppure per analizzare il suo contenuto. In questo articolo, esploreremo come leggere un file di testo in C++ usando metodi semplici ma efficaci.

## Come leggere un file di testo in C++

Per prima cosa, dobbiamo includere la libreria standard per la gestione dei file, ```<fstream>```. Utilizzando questa libreria, possiamo aprire il file di testo specificando il suo percorso e il suo nome usando la funzione ```open()```. Assicurati di avere il file nella stessa cartella del tuo programma, altrimenti dovrai specificare il percorso completo del file.

Una volta aperto il file, possiamo leggere il suo contenuto usando la funzione ```getline()```, che legge una riga alla volta fino a raggiungere il carattere di newline. Questo ci permette di leggere il contenuto del file e stamparlo a schermo usando la funzione ```cout```. Ad esempio:

```C++
#include <iostream>
#include <fstream>

using namespace std;

int main() {
    
    // Apriamo il file in modalità lettura
    ifstream file("esempio.txt");
    
    // Leggiamo il contenuto del file ed eseguiamo un'azione su ogni riga
    string riga;
    while (getline(file, riga)) {
        // Stampiamo la riga a schermo
        cout << riga << endl;
    }
    
    // Chiudiamo il file
    file.close();
    
    return 0;
}
```

Il risultato sarà l'output di ogni riga del file. Assicurati di aggiungere un'istruzione ```cout``` per stampare eventuali errori di apertura del file.

## Approfondimento sulla lettura di file di testo

Esistono diverse funzioni e metodi per leggere un file di testo in C++, come l'utilizzo di stream di input e output o l'utilizzo della funzione ```get()``` per leggere carattere per carattere. Puoi esplorare questi metodi per trovare quello che meglio si adatta alle tue esigenze.

## Vedi anche

- [Come scrivere un file di testo in C++](link to article in Italian)
- [Gestione degli errori durante la lettura di file in C++](link to article in Italian)
- [Come leggere e scrivere file binari in C++](link to article in Italian)