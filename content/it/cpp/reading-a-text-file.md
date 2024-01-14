---
title:    "C++: Lettura di un file di testo"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché
L'azione di leggere un file di testo può sembrare semplice e banale, ma è una delle componenti fondamentali della programmazione in C++. Ad esempio, questa operazione può essere utile per analizzare grandi quantità di dati o per salvare e caricare informazioni di configurazione per un'applicazione. Anticipando i possibili scenari in cui si dovrà leggere un file di testo, è importante conoscere il processo e le tecniche per farlo in modo efficiente.

## Come fare
Per prima cosa, è necessario dichiarare un oggetto di tipo *ifstream*, che consentirà di aprire e leggere il file. È importante specificare il percorso corretto del file o utilizzare la funzione *std::ifstream::open* per specificare il percorso al momento dell'esecuzione del programma. Una volta aperto il file, è possibile utilizzare il metodo *std::ifstream::getline* per leggere ogni riga di testo fino a quando non si raggiunge la fine del file. L'esempio seguente mostra come leggere un file di testo contenente una lista di nomi e stamparli sulla console:

```C++
#include<iostream>
#include<fstream>
using namespace std;

int main() {
  // Dichiarazione del file da leggere
  ifstream file("nome_file.txt");

  // Variabile per contenere il nome letto
  string nome;

  // Ciclo di lettura fino alla fine del file
  while(getline(file, nome)) {
    // Stampa il nome sulla console
    cout << nome << endl;
  }

  // Chiusura del file
  file.close();
  return 0;
}
```
**Output**:
```
Maria
Giovanni
Sara
Luca
```

## Approfondimento
Oltre a leggere semplicemente il contenuto di un file, esistono anche altre funzioni utili per manipolare i dati letti. Ad esempio, si possono utilizzare le funzioni *std::ifstream::tellg* e *std::ifstream::seekg* per ottenere e impostare la posizione corrente del cursore di lettura nel file. Inoltre, è possibile utilizzare il metodo *std::ifstream::good* per verificare se l'operazione di lettura è andata a buon fine. È importante anche considerare la gestione degli errori durante la lettura del file, ad esempio verificando la corretta apertura del file e gestendo eventuali eccezioni.

## Vedi anche
- [C++ File Input/Output](https://www.tutorialspoint.com/cplusplus/cpp_files_streams.htm)
- [std::ifstream reference](https://www.cplusplus.com/reference/fstream/ifstream/)
- [Manipolazione dei file in C++](https://www.techiedelight.com/file-handling-cpp-create-read-write/)