---
title:                "Lavorare con json"
html_title:           "C++: Lavorare con json"
simple_title:         "Lavorare con json"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/working-with-json.md"
---

{{< edit_this_page >}}

# Cosa e perche?

Lavorare con JSON significa manipolare dati in un formato leggibile da computer, ma anche facilmente comprensibile dagli umani. I programmatori spesso usano JSON per lo scambio di dati tra diverse applicazioni o per la memorizzazione di dati strutturati.

# Come:

Ecco come si può lavorare con JSON utilizzando C++:

```C++
// Esempio di codice per creare un oggetto JSON
#include <iostream>
#include <nlohmann/json.hpp>
using json = nlohmann::json; // alias

json oggetto = {
    {"nome", "Mario"},
    {"eta", 25},
    {"hobby", {"programmazione", "gaming", "cucina"}}
};

// Esempio di codice per leggere dati da un file JSON
#include <iostream>
#include <fstream>
#include <nlohmann/json.hpp>
using json = nlohmann::json; // alias

int main() {
    // Aprire il file contenente il JSON
    std::ifstream input("dati.json");
    // Creare un oggetto JSON per leggere i dati
    json dati = json::parse(input);

    // Accedere ai dati
    std::cout << "Citta preferita: " << dati["citta"] << std::endl;
    std::cout << "Libri preferiti:" << std::endl;
    for (auto libro : dati["libri"]) {
        std::cout << " - " << libro["titolo"] << " di " << libro["autore"] << std::endl;
    }
    return 0;
}
```

Esempio di output:

```
Citta preferita: Milano
Libri preferiti:
 - 1984 di George Orwell
 - Il processo di Franz Kafka
```

# Approfondimenti

- JSON è basato sul formato di dati JavaScript, ma può essere utilizzato con diversi linguaggi di programmazione.
- Altri formati per lo scambio di dati sono XML e CSV, ma JSON è spesso preferito per la sua semplicità e leggibilità.
- Il codice per lavorare con JSON è spesso poco complicato e ben supportato da librerie esterne.

# Vedi anche

- [Sito ufficiale di JSON](https://www.json.org/)
- [Documentazione di nlohmann/json per C++](https://github.com/nlohmann/json#examples)