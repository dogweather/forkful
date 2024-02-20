---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:50.216754-07:00
description: "JSON (JavaScript Object Notation) \xE8 un formato leggero per memorizzare\
  \ e trasportare dati, rendendolo un eccellente mezzo per lo scambio di dati tra\u2026"
lastmod: 2024-02-19 22:05:02.822881
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation) \xE8 un formato leggero per memorizzare\
  \ e trasportare dati, rendendolo un eccellente mezzo per lo scambio di dati tra\u2026"
title: Lavorare con JSON
---

{{< edit_this_page >}}

## Cosa & Perché?

JSON (JavaScript Object Notation) è un formato leggero per memorizzare e trasportare dati, rendendolo un eccellente mezzo per lo scambio di dati tra server e applicazioni web. I programmatori utilizzano JSON per la sua facile leggibilità da parte degli umani e la semplicità di analisi da parte delle macchine, specialmente quando si lavora su applicazioni che richiedono lo scambio di dati su internet o impostazioni di configurazione.

## Come fare:

In C++, non c'è un supporto nativo per JSON, ma librerie di terze parti come nlohmann/json rendono il processo diretto. Ecco come utilizzarla per attività di base:

Prima, assicurati di avere installato la libreria. Se stai usando un gestore di pacchetti come vcpkg o Conan, puoi facilmente aggiungere `nlohmann/json` al tuo progetto.

### Analizzare JSON da una stringa

```cpp
#include <iostream>
#include <nlohmann/json.hpp>

int main() {
    // Dati JSON come stringa
    std::string jsonData = "{\"name\":\"John\", \"age\":30, \"city\":\"New York\"}";

    // Analizza la stringa JSON
    auto jsonObject = nlohmann::json::parse(jsonData);

    // Accesso ai dati
    std::cout << "Nome: " << jsonObject["name"] << "\n"
              << "Età: " << jsonObject["age"] << "\n"
              << "Città: " << jsonObject["city"] << std::endl;

    return 0;
}
```

**Output dell'esempio:**

```
Nome: John
Età: 30
Città: New York
```

### Generare JSON

Creare dati JSON è altrettanto semplice; basta assegnare valori a un oggetto `nlohmann::json`.

```cpp
#include <nlohmann/json.hpp>
#include <iostream>

int main() {
    // Creare un oggetto JSON
    nlohmann::json jsonObject;
    jsonObject["name"] = "Jane";
    jsonObject["age"] = 25;
    jsonObject["city"] = "Los Angeles";

    // Convertire l'oggetto JSON in stringa e stampare
    std::string jsonString = jsonObject.dump(4); // Argomento 4 per la stampa formattata
    std::cout << jsonString << std::endl;

    return 0;
}
```

**Output dell'esempio:**

```
{
    "name": "Jane",
    "age": 25,
    "city": "Los Angeles"
}
```

Questi esempi dimostrano la funzionalità di base per lavorare con JSON in C++ utilizzando la libreria `nlohmann/json`. Con queste basi, puoi analizzare e generare JSON per varie applicazioni, dai file di configurazione allo scambio di dati in applicazioni di rete.
