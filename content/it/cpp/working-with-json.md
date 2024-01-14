---
title:                "C++: Lavorare con json"
simple_title:         "Lavorare con json"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/working-with-json.md"
---

{{< edit_this_page >}}

### Perché

JSON è uno dei formati di dati più popolari nel mondo della programmazione e delle applicazioni web. Saper lavorare con JSON può essere estremamente utile per gestire e scambiare dati tra diverse applicazioni o sistemi.

### Come fare

Per lavorare con JSON in C++, è necessario utilizzare una libreria come "nlohmann/json" che permetta di manipolare facilmente i dati in formato JSON. Ecco un esempio di codice che mostra come creare un oggetto JSON e stamparne il contenuto:

```C++
#include <iostream>
#include <nlohmann/json.hpp>

int main()
{
  // Creazione di un oggetto JSON
  nlohmann::json data = {
    {"nome", "Marco"},
    {"età", 25},
    {"sesso", "maschio"}
  };

  // Stampa del contenuto dell'oggetto JSON
  std::cout << data.dump(4) << std::endl;

  return 0;
}
```

Il risultato di questo codice sarà:

```C++
{
    "nome": "Marco",
    "età": 25,
    "sesso": "maschio"
}
```

È anche possibile leggere ed estrarre i dati da un file JSON utilizzando la funzione `nlohmann::json::parse` e accedendo ai dati come ad un normale oggetto C++.

### Approfondimento

È importante notare che esistono diverse librerie per lavorare con JSON in C++, ognuna con i propri vantaggi e svantaggi. È quindi consigliabile fare una ricerca e scegliere quella più adatta alle proprie esigenze.

Inoltre, è possibile utilizzare altre funzioni della libreria "nlohmann/json" per manipolare e accedere ai dati in modo più avanzato, come ad esempio la ricerca e la modifica di specifici elementi all'interno di un oggetto JSON.

### Vedi anche

- Documentazione ufficiale di nlohmann/json: https://github.com/nlohmann/json
- Una guida introduttiva su come utilizzare JSON in C++: https://www.techiedelight.com/use-json-cpp-standard-library-cpprestsdk/