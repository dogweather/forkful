---
title:                "C++: Lavorare con i file csv"
simple_title:         "Lavorare con i file csv"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## Perché
Il CSV, o Comma Separated Values, è un formato di file ampiamente utilizzato per archiviare dati in modo tabellare. In questo articolo, esploreremo come lavorare con i file CSV utilizzando il linguaggio di programmazione C++ e perché potrebbe essere utile farlo.

## Come fare
Per iniziare a lavorare con i file CSV in C++, è necessario includere la libreria `ifstream` per gestire i file di input e `sstream` per analizzare i dati all'interno del file. Qui di seguito è riportato un esempio di codice che legge un file CSV di nomi e età e li stampa a schermo:

```C++
#include <iostream>
#include <fstream>
#include <sstream>

using namespace std;

int main() {
  // apriamo il file in modalità di lettura
  ifstream file("persone.csv");
  // verifichiamo se il file è stato aperto correttamente
  if (!file.is_open()) {
      cout << "Errore nell'apertura del file";
      return 0;
  }

  // leggiamo il file riga per riga e stampiamo i dati
  string riga, nome;
  int eta;
  while (getline(file, riga)) {
      // utilizziamo un stringstream per analizzare i dati all'interno della riga
      stringstream ss(riga);
      // leggiamo il nome e l'età dalla riga e li stampiamo
      getline(ss, nome, ',');
      ss >> eta;
      cout << nome << " ha " << eta << " anni" << endl;
  }

  // chiudiamo il file
  file.close();
  return 0;
}
```

Ora supponiamo che il nostro file CSV abbia il seguente contenuto:
```
Maria, 23
Luigi, 34
Sara, 27
```

L'output di questo codice sarebbe:
```
Maria ha 23 anni
Luigi ha 34 anni
Sara ha 27 anni
```

## Approfondimento
Oltre alla semplice lettura e stampa dei dati da un file CSV, è possibile utilizzare il linguaggio C++ per analizzare, manipolare e gestire i dati in modo più accurato. Ad esempio, si potrebbe utilizzare la libreria `ofstream` per scrivere nuovi dati in un file CSV, oppure la libreria `array` per gestire i dati in modo più efficiente.
Inoltre, è importante notare che il formato CSV non è standardizzato, quindi è possibile che altri applicativi o librerie abbiano diverse implementazioni per la gestione dei file CSV.

## Vedi anche
- [Documentazione di C++](https://devdocs.io/cpp/)
- [Tutorial su CSV in C++](https://www.youtube.com/watch?v=6A-XUSqyF2E)
- [Libreria ofstream di C++](https://www.cplusplus.com/reference/fstream/ofstream/)
- [Gestione dei file in C++](https://www.geeksforgeeks.org/file-handling-c-classes/)