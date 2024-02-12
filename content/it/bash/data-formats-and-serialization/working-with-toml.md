---
title:                "Lavorare con TOML"
aliases:
- /it/bash/working-with-toml.md
date:                  2024-01-26T04:19:06.409958-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/working-with-toml.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
TOML, acronimo di Tom's Obvious, Minimal Language (Linguaggio Minimo e Ovvio di Tom), è un formato di serializzazione dei dati. I programmatori lo adorano per la sua semplicità e leggibilità; è eccellente per file di configurazione, ha un'atmosfera simile a YAML ma meno ingombrante di JSON per un essere umano.

## Come Fare:
Prima di tutto, installare `toml-cli` per giocare con TOML in Bash. Utile per leggere o modificare file TOML al volo.

```Bash
# Installa toml-cli, il nostro piccolo aiutante per le attività TOML
pip install toml-cli

# Immagina di avere un file TOML, 'config.toml'
echo -e 'title = "Demo TOML"\n\n[owner]\nname = "Tom"\ndob = 1979-05-27T07:32:00Z' > config.toml

# Leggi un valore
toml get config.toml owner.name
# Output: Tom

# Imposta un valore
toml set config.toml 'owner.dob' '2000-01-01T00:00:00Z'
# Suggerimento: Usa le virgolette per chiavi con punti o caratteri strani!
```

## Approfondimento
Nato dall'antipatia per gli ostacoli di JSON per gli umani, TOML è apparso nel 2013. Tom Preston-Werner, co-fondatore di GitHub, voleva qualcosa di super leggibile. YAML e INI erano alternative ma TOML è come il meglio di entrambi.

Bam, hai dati nidificati e array, senza le insidie di YAML e le parentesi graffe di JSON. Ora TOML è la scelta principale per la configurazione nel Cargo di Rust, il che parla del suo aumento nel mondo degli sviluppatori. È guidato da una specifica, mantenendo tutto stretto e ben definito. Troverai parser in quasi ogni linguaggio, rendendolo ampiamente adottabile.

## Vedi Anche
- Repo GitHub Ufficiale di TOML: https://github.com/toml-lang/toml
- toml-cli su PyPI: https://pypi.org/project/toml-cli/
- Confronto dei formati di serializzazione dei dati: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
