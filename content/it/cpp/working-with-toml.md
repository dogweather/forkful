---
title:                "Lavorare con TOML"
date:                  2024-01-26T04:19:29.869297-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con TOML"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/working-with-toml.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?
TOML (Tom's Obvious, Minimal Language - Linguaggio Minimale ed Ovvio di Tom) è un formato di serializzazione di dati facile da leggere grazie alla sua semantica chiara. I programmatori usano TOML per file di configurazione perché offre un equilibrio tra leggibilità umana e analizzabilità da parte della macchina.

## Come fare:
Per lavorare con TOML in C++, avrai bisogno di una libreria come `toml++`. Ecco una guida rapida:

```C++
#include <toml++/toml.h>
#include <iostream>
#include <fstream>

int main() {
    // Analizza il TOML da un file
    std::ifstream ifs("config.toml");
    auto config = toml::parse(ifs);

    // Accesso a un valore
    std::string title = config["title"].value_or("Senza titolo");
    std::cout << "Titolo: " << title << '\n';

    // Modifica e salva il TOML
    config["title"] = "Nuovo Titolo";
    std::ofstream ofs("config.toml");
    ofs << config;
}
```

Esempio di `config.toml`:
```toml
title = "Example"
```

Esempio di output:
```plaintext
Titolo: Example
```

## Approfondimento
TOML è stato creato da Tom Preston-Werner nel 2013 come alternativa a YAML e JSON. È progettato per essere semplice ed esplicito, principalmente per file di configurazione. A differenza di JSON, TOML si concentra sull'essere non ambiguo, il che significa che è deterministico nel modo in cui il documento viene analizzato.

Tra le alternative a TOML c'è YAML, che è più permissivo in ciò che è consentito, anche se a volte a scapito della prevedibilità. JSON, un'altra alternativa, ha una struttura piuttosto rigorosa ma non è altrettanto amichevole per le configurazioni umane a causa della mancanza di commenti e della sua sintassi piena di parentesi graffe.

Nella sua implementazione, `toml++` è una libreria C++17 solo intestazione che è conforme all'ultima specifica TOML. Fornisce un'interfaccia simile a DOM per navigare e manipolare dati TOML, rendendolo semplice da integrare nei progetti. La libreria si occupa dell'analisi, della validazione e della generazione dell'output, permettendoti di ottenere e impostare dati TOML usando tipi C++.

## Vedi anche
- Il repository GitHub di TOML: https://github.com/toml-lang/toml
- `toml++`, una libreria C++ per TOML: https://github.com/marzer/tomlplusplus
- La documentazione ufficiale di TOML con spiegazioni dettagliate del formato: https://toml.io/en/
