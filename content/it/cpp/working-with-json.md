---
title:                "Lavorare con JSON"
html_title:           "Arduino: Lavorare con JSON"
simple_title:         "Lavorare con JSON"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON, ovvero JavaScript Object Notation, è un formato leggero per lo scambio di dati. Programmatori lo usano per la sua leggibilità e semplicità nell'integrazione con sistemi di vario tipo, tra cui API web e configurazioni.

## How to:
Per lavorare con JSON in C++, puoi usare la libreria `nlohmann/json`. Ecco un esempio rapido:

```C++
#include <iostream>
#include <nlohmann/json.hpp>

int main() {
    // Crea un oggetto JSON
    nlohmann::json obj;
    obj["nome"] = "Mario";
    obj["eta"] = 30;
    obj["hobbies"] = {"calcio", "videogiochi", "viaggiare"};

    // Stampa l'oggetto JSON in formato stringa
    std::cout << obj.dump(4) << std::endl;

    return 0;
}
```

Output:
```json
{
    "eta": 30,
    "hobbies": [
        "calcio",
        "videogiochi",
        "viaggiare"
    ],
    "nome": "Mario"
}
```

## Deep Dive:
JSON è nato nel 2001, ideato da Douglas Crockford. Come alternativa a XML, è molto più snello, facile da leggere e scrivere per l'uomo, nonché semplice da analizzare e generare per le macchine. In C++ ci sono diverse librerie per gestire JSON, inclusi `nlohmann/json`, `jsoncpp`, `RapidJSON`, ognuna con le sue particolarità in termini di performance, compatibilità e facilità d'uso.

## See Also:
- Documentazione `nlohmann/json`: https://github.com/nlohmann/json
- JsonCpp GitHub Repository: https://github.com/open-source-parsers/jsoncpp
- RapidJSON GitHub Repository: https://github.com/Tencent/rapidjson
- Confronto tra librerie JSON in C++: https://www.json.org/json-en.html
