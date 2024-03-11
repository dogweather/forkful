---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:12:19.043265-07:00
description: "TOML (Tom's Obvious, Minimal Language, ovvero Linguaggio Minimale e\
  \ Ovvio di Tom) \xE8 un formato di file di configurazione che \xE8 facile da leggere\
  \ grazie\u2026"
lastmod: '2024-03-11T00:14:17.553342-06:00'
model: gpt-4-0125-preview
summary: "TOML (Tom's Obvious, Minimal Language, ovvero Linguaggio Minimale e Ovvio\
  \ di Tom) \xE8 un formato di file di configurazione che \xE8 facile da leggere grazie\u2026"
title: Lavorare con TOML
---

{{< edit_this_page >}}

## Cosa e Perché?

TOML (Tom's Obvious, Minimal Language, ovvero Linguaggio Minimale e Ovvio di Tom) è un formato di file di configurazione che è facile da leggere grazie alla sua chiara semantica. I programmatori lo utilizzano per i file di configurazione nelle applicazioni perché la sua semplicità e leggibilità umana lo rendono un'ottima scelta rispetto a formati come XML o JSON in certi contesti.

## Come fare:

Per lavorare con TOML in C, hai prima bisogno di una libreria in grado di analizzare i file TOML, dato che la libreria standard C non include questa funzionalità. Una scelta popolare è `tomlc99`, un parser TOML leggero per C99. Ecco una rapida guida per leggere un semplice file di configurazione TOML:

Prima, assicurati di avere installato e correttamente collegato `tomlc99` nel tuo progetto.

**Esempio di file TOML (`config.toml`):**
```toml
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

**Codice C per analizzare questo file:**

```c
#include <stdio.h>
#include <stdlib.h>
#include "toml.h"

int main() {
    FILE *configFile;
    configFile = fopen("config.toml", "r");
    if (!configFile) {
        perror("Impossibile aprire il file");
        return EXIT_FAILURE;
    }

    toml_table_t *config = toml_parse_file(configFile, NULL, 0);
    if (!config) {
        fprintf(stderr, "Errore nell'analisi del file\n");
        fclose(configFile);
        return EXIT_FAILURE;
    }

    toml_table_t *database = toml_table_in(config, "database");
    if (database) {
        const char *server = toml_raw_in(database, "server");
        printf("Server Database: %s\n", server);

        toml_array_t *ports = toml_array_in(database, "ports");
        for (int i = 0; i < toml_array_nelem(ports); i++) {
            int64_t port;
            toml_int_at(ports, i, &port);
            printf("Porta %d: %ld\n", i, port);
        }
    }

    toml_free(config);
    fclose(configFile);
    return EXIT_SUCCESS;
}
```

**Output:**
```
Server Database: "192.168.1.1"
Porta 0: 8001
Porta 1: 8001
Porta 2: 8002
```

## Approfondimento

TOML è stato creato da Tom Preston-Werner, co-fondatore di GitHub, come risposta alle limitazioni che percepiva in altri formati di file di configurazione. Il suo obiettivo è quello di essere diretto e non ambiguo, sia per gli umani che per i computer, da leggere e scrivere senza aver bisogno di regole di analisi complesse. Nel ecosistema C, TOML non è un cittadino di prima classe come potrebbe esserlo in linguaggi di livello più alto come Rust con il suo `serde_toml` o Python con `toml`, che dispongono di librerie con supporto nativo. Piuttosto, gli sviluppatori C devono affidarsi a librerie esterne come `tomlc99`, ma questo è tipico data l'enfasi di C sul minimalismo e le prestazioni.

Sebbene TOML sia lodato per la sua chiarezza, quando si sceglie un formato di file di configurazione, è vitale considerare le necessità del progetto. In scenari che richiedono strutture più complesse o interattività con web API, JSON o anche YAML potrebbero offrire una migliore adattabilità nonostante la loro maggiore complessità. TOML brilla nelle configurazioni dove leggibilità e semplicità sono di massima importanza, non necessariamente dove sono necessarie le strutture dati più avanzate.
