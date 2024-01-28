---
title:                "Praca z TOML"
date:                  2024-01-26T04:19:45.765022-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z TOML"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/working-with-toml.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
TOML jest językiem serializacji danych zaprojektowanym, by był łatwy do odczytu i zapisu. Programiści używają go dla plików konfiguracyjnych, prostego przechowywania danych i wymiany danych między językami ze względu na jego przejrzystość i przyjazność dla człowieka.

## Jak to zrobić:
Przeparsujmy plik konfiguracyjny TOML w C, używając biblioteki "tomlc99". Najpierw zainstaluj bibliotekę. Następnie, utwórz `config.toml`:

```toml
title = "Przykład TOML"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
```

Teraz przeparsuj go w C:

```c
#include <stdio.h>
#include "toml.h"

int main() {
    FILE* fp;
    char errbuf[200];

    if (0 == (fp = fopen("config.toml", "r"))) {
        printf("Błąd: nie można otworzyć pliku konfiguracyjnego\n");
        return 1;
    }
    
    toml_table_t* conf = toml_parse_file(fp, errbuf, sizeof(errbuf));
    fclose(fp);
    if (0 == conf) {
        printf("Błąd: %s\n", errbuf);
        return 1;
    }

    printf("Tytuł: %s\n", toml_raw_in(conf, "title"));

    toml_table_t* owner = toml_table_in(conf, "owner");
    printf("Nazwa Właściciela: %s\n", toml_raw_in(owner, "name"));

    toml_free(conf);
    return 0;
}
```
Przykładowe wyjście:
```
Tytuł: "Przykład TOML"
Nazwa Właściciela: "Tom Preston-Werner"
```

## Szczegółowe omówienie
TOML, co oznacza Tom's Obvious, Minimal Language (Oczywisty, Minimalny Język Toma), został stworzony przez Toma Preston-Wernera w 2013 roku. Służy jako prostsza alternatywa dla formatów takich jak XML i YAML, koncentrując się na byciu bardziej czytelnym i zapisywalnym dla ludzi. Chociaż JSON jest inną alternatywą, TOML zachowuje strukturę, która jest łatwiejsza do wizualnego przetworzenia przez ludzi, co jest jednym z głównych powodów jego adopcji w plikach konfiguracyjnych.

W C, praca z TOML wiąże się z wyborem biblioteki analizującej, ponieważ język nie obsługuje go natywnie. Biblioteki takie jak "tomlc99" są zgodne z C99 i zapewniają API do dekodowania tekstu TOML. Biorąc pod uwagę wydajność, właściwe obsługiwanie błędów i zarządzanie pamięcią są kluczowe, ponieważ C nie posiada wbudowanego systemu oczyszczania pamięci (garbage collection).

## Zobacz także:
1. Specyfikacja TOML: [https://toml.io/en/](https://toml.io/en/)
2. Repozytorium GitHub tomlc99: [https://github.com/cktan/tomlc99](https://github.com/cktan/tomlc99)
3. Porównanie formatów serializacji danych: [https://labs.eleks.com/2015/07/comparison-of-data-serialization-formats.html](https://labs.eleks.com/2015/07/comparison-of-data-serialization-formats.html)
