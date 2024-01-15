---
title:                "Lavorare con file csv"
html_title:           "C++: Lavorare con file csv"
simple_title:         "Lavorare con file csv"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui potresti voler lavorare con file CSV in C++. Ad esempio, può essere necessario importare o esportare dati da un database o creare report da un foglio di calcolo. Inoltre, CSV è un formato di file molto comune e semplice da utilizzare, quindi è importante saper manipolarlo in modo efficace nel tuo codice.

## Come Fare

Per iniziare a lavorare con file CSV in C++, è necessario includere la libreria standard `<fstream>` e utilizzare gli oggetti `ifstream` e `ofstream` per leggere e scrivere rispettivamente. Ecco un esempio di codice che legge un file CSV e stampa il suo contenuto:

```C++
#include <fstream>
#include <iostream>

using namespace std;

int main() {
  ifstream file("data.csv");
  if (file.is_open()) {
    string line;
    // Cicla fino alla fine del file
    while (getline(file, line)) {
      // Stampa la riga corrente
      cout << line << endl;
    }
    file.close();
  } else {
    // Gestione dell'errore se il file non può essere aperto
    cout << "Errore nell'apertura del file" << endl;
  }

  return 0;
}
```

Se il file CSV ha intestazioni di colonna, è possibile utilizzare la funzione `getline` e la classe `stringstream` per dividere ogni riga in base alle virgole (o al carattere delimitatore scelto) e manipolare i dati come si desidera. Ad esempio, il codice seguente legge un file CSV con tre colonne e stampa la prima colonna di ogni riga:

```C++
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>

using namespace std;

int main() {
  ifstream file("data.csv");
  if (file.is_open()) {
    string line, col1, col2, col3;
    // Cicla fino alla fine del file
    while (getline(file, line)) {
      // Utilizza un oggetto stringstream per dividere la riga in base alle virgole
      stringstream ss(line);
      // Assegna i valori alle variabili delle colonne
      getline(ss, col1, ','); // Prima colonna
      getline(ss, col2, ','); // Seconda colonna
      getline(ss, col3, ','); // Terza colonna
      // Stampa la prima colonna
      cout << col1 << endl;
    }
    file.close();
  } else {
    // Gestione dell'errore se il file non può essere aperto
    cout << "Errore nell'apertura del file" << endl;
  }

  return 0;
}
```

## Approfondimento

CSV è un formato di file molto comune e facile da usare, ma ci sono alcune cose importanti da tenere a mente quando si lavora con esso in C++. In primo luogo, assicurati di gestire correttamente i casi in cui le celle contengono virgole o altri caratteri delimitatori. Inoltre, è possibile utilizzare la libreria esterna `csv.h` per semplificare il processo di lettura e scrittura dei file CSV.

## Vedi Anche

- [Documentazione ufficiale di C++](https://en.cppreference.com/)
- [Esempi di uso delle classi `ifstream` e `ofstream`](https://www.geeksforgeeks.org/ifstream-ofstream-classes-c/)
- [Libreria esterna `csv.h`](https://github.com/ben-strasser/fast-cpp-csv-parser)