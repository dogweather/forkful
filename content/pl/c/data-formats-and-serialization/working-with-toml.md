---
aliases:
- /pl/c/working-with-toml/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:12:45.775556-07:00
description: "TOML (Tom's Obvious, Minimal Language) to format pliku konfiguracyjnego,\
  \ kt\xF3ry jest \u0142atwy do odczytania dzi\u0119ki swojej jasnej semantyce. Programi\u015B\
  ci\u2026"
lastmod: 2024-02-18 23:08:50.105245
model: gpt-4-0125-preview
summary: "TOML (Tom's Obvious, Minimal Language) to format pliku konfiguracyjnego,\
  \ kt\xF3ry jest \u0142atwy do odczytania dzi\u0119ki swojej jasnej semantyce. Programi\u015B\
  ci\u2026"
title: Praca z TOML
---

{{< edit_this_page >}}

## Co i dlaczego?

TOML (Tom's Obvious, Minimal Language) to format pliku konfiguracyjnego, który jest łatwy do odczytania dzięki swojej jasnej semantyce. Programiści używają go do plików konfiguracyjnych w aplikacjach, ponieważ jego prostota i czytelność dla człowieka sprawiają, że jest doskonałym wyborem w stosunku do formatów takich jak XML czy JSON w pewnych kontekstach.

## Jak używać:

Aby pracować z TOML w C, potrzebujesz najpierw biblioteki zdolnej do analizowania plików TOML, ponieważ standardowa biblioteka C nie zawiera tej funkcjonalności. Popularnym wyborem jest `tomlc99`, lekki parser TOML dla C99. Oto krótki przewodnik, jak odczytać prosty plik konfiguracyjny TOML:

Najpierw upewnij się, że masz zainstalowanego i odpowiednio połączonego w projekcie `tomlc99`.

**Przykładowy plik TOML (`config.toml`):**
```toml
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

**Kod C do analizy tego pliku:**

```c
#include <stdio.h>
#include <stdlib.h>
#include "toml.h"

int main() {
    FILE *configFile;
    configFile = fopen("config.toml", "r");
    if (!configFile) {
        perror("Nie można otworzyć pliku");
        return EXIT_FAILURE;
    }

    toml_table_t *config = toml_parse_file(configFile, NULL, 0);
    if (!config) {
        fprintf(stderr, "Błąd podczas analizowania pliku\n");
        fclose(configFile);
        return EXIT_FAILURE;
    }

    toml_table_t *database = toml_table_in(config, "database");
    if (database) {
        const char *server = toml_raw_in(database, "server");
        printf("Serwer bazy danych: %s\n", server);

        toml_array_t *ports = toml_array_in(database, "ports");
        for (int i = 0; i < toml_array_nelem(ports); i++) {
            int64_t port;
            toml_int_at(ports, i, &port);
            printf("Port %d: %ld\n", i, port);
        }
    }

    toml_free(config);
    fclose(configFile);
    return EXIT_SUCCESS;
}
```

**Wynik:**
```
Serwer bazy danych: "192.168.1.1"
Port 0: 8001
Port 1: 8001
Port 2: 8002
```

## Pogłębiona analiza

TOML został stworzony przez Toma Preston-Wernera, współzałożyciela GitHuba, jako odpowiedź na ograniczenia, które dostrzegał w innych formatach plików konfiguracyjnych. Jego celem jest bycie prostym i jednoznacznym, zarówno dla ludzi, jak i komputerów, do czytania i pisania bez potrzeby skomplikowanych zasad analizy. W ekosystemie C, TOML nie jest obywatelem pierwszej kategorii tak jak może być to w językach wyższego poziomu takich jak Rust z jego `serde_toml` czy Python z `toml`, które mają biblioteki z natywnym wsparciem. Zamiast tego deweloperzy C muszą polegać na zewnętrznych bibliotekach takich jak `tomlc99`, ale jest to typowe biorąc pod uwagę nacisk C na minimalizm i wydajność.

Chociaż TOML jest chwalony za swoją jasność, przy wyborze formatu pliku konfiguracyjnego ważne jest, aby rozważyć potrzeby projektu. W scenariuszach wymagających bardziej złożonych struktur lub interakcji z internetowymi API, JSON czy nawet YAML mogą lepiej pasować pomimo ich zwiększonej złożoności. TOML świeci w konfiguracjach, gdzie czytelność i prostota są najważniejsze, niekoniecznie tam, gdzie potrzebne są najbardziej zaawansowane struktury danych.
