---
aliases:
- /pl/c/working-with-yaml/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:13:47.830292-07:00
description: "YAML, kt\xF3ry oznacza \"YAML Ain't Markup Language\" (YAML nie jest\
  \ j\u0119zykiem znacznik\xF3w), to standard serializacji danych czytelnych dla cz\u0142\
  owieka, kt\xF3ry mo\u017Ce\u2026"
lastmod: 2024-02-18 23:08:50.102085
model: gpt-4-0125-preview
summary: "YAML, kt\xF3ry oznacza \"YAML Ain't Markup Language\" (YAML nie jest j\u0119\
  zykiem znacznik\xF3w), to standard serializacji danych czytelnych dla cz\u0142owieka,\
  \ kt\xF3ry mo\u017Ce\u2026"
title: Praca z YAML
---

{{< edit_this_page >}}

## Co i dlaczego?

YAML, który oznacza "YAML Ain't Markup Language" (YAML nie jest językiem znaczników), to standard serializacji danych czytelnych dla człowieka, który może być używany w różnych aplikacjach, od plików konfiguracyjnych po przechowywanie danych. Programiści często pracują z YAML, kiedy potrzebują łatwego do odczytu i łatwego do zapisu formatu dla plików konfiguracyjnych lub wymiany danych między językami i systemami.

## Jak to zrobić:

Praca z YAML w C wymaga biblioteki, ponieważ standardowa biblioteka języka C nie oferuje bezpośredniego wsparcia dla parsowania lub serializacji YAML. Jedną z najpopularniejszych bibliotek YAML dla C jest `libyaml`, która oferuje zarówno interfejsy niskiego, jak i wysokiego poziomu do parsowania i emitowania YAML. Poniżej znajduje się przykład, jak przeanalizować prosty plik YAML przy użyciu `libyaml`:

**Po pierwsze**, musisz zainstalować bibliotekę `libyaml`. Jeśli jesteś na systemie podobnym do Unix, zwykle możesz ją zainstalować za pomocą menedżera pakietów. Na przykład na Ubuntu:

```bash
sudo apt-get install libyaml-dev
```

**Następnie**, rozważ prosty plik YAML o nazwie `config.yaml`:

```yaml
name: John Doe
age: 29
married: false
```

**Oto** podstawowy przykład, jak przeanalizować ten plik YAML w C:

```c
#include <yaml.h>
#include <stdio.h>
#include <stdlib.h>

void process_yaml_file(const char *filename) {
    FILE *fh = fopen(filename, "rb");
    yaml_parser_t parser;
    yaml_event_t event;

    if (!yaml_parser_initialize(&parser))
        fputs("Nie udało się zainicjować parsera YAML!\n", stderr);

    if (fh == NULL)
        fputs("Nie udało się otworzyć pliku!\n", stderr);

    yaml_parser_set_input_file(&parser, fh);

    while (1) {
        if (!yaml_parser_parse(&parser, &event))
            break;

        if (event.type == YAML_SCALAR_EVENT) {
            printf("Wartość: %s\n", event.data.scalar.value);
        }

        if (event.type == YAML_STREAM_END_EVENT)
            break;

        yaml_event_delete(&event);
    }

    yaml_parser_delete(&parser);
    fclose(fh);
}

int main() {
    process_yaml_file("config.yaml");
    return 0;
}
```

Ten prosty program otwiera plik YAML, inicjuje parser YAML i czyta plik, wyświetlając wartości skalarne (w tym przykładzie, pola naszego prostego YAML). Należy zauważyć, że sprawdzanie błędów jest minimalne w tym prostym przykładzie i powinno być bardziej rozbudowane w kodzie produkcyjnym.

Uruchomienie programu z naszym `config.yaml` da następujące wyjście:

```plaintext
Wartość: John Doe
Wartość: 29
Wartość: false
```

## Pogłębiona analiza

YAML został po raz pierwszy wydany w 2001 roku i zaprojektowany tak, aby był bardziej czytelny i przyjazny dla użytkownika niż inne formaty serializacji danych, takie jak XML czy JSON, czerpiąc z kilku języków, takich jak C, Perl i Python, dla swojej filozofii projektowej. Pomimo swoich zalet w czytelności i łatwości ręcznej modyfikacji, YAML może być skomplikowany do programistycznego analizowania ze względu na swoją zależność od wcięć i obszernego zestawu funkcji, w tym odniesień i typów niestandardowych.

Chociaż `libyaml` zapewnia solidny, niskopoziomowy dostęp do parsowania i emitowania YAML w C, może być uciążliwa w prostych zadaniach ze względu na jej rozwlekłe API. Z tych powodów niektórzy programiści wolą korzystać z bibliotek wyższego poziomu lub nawet innych formatów serializacji danych, takich jak JSON, podczas pracy w C, zwłaszcza gdy priorytetem jest wydajne parsowanie z minimalnym nadmiarem kodu. Jednak YAML pozostaje popularnym wyborem dla plików konfiguracyjnych i sytuacji, gdy czytelność dla człowieka jest najważniejsza. Alternatywy takie jak TinyYAML, czy też osadzanie interpreterów wysokiego poziomu (np. osadzenie Pythona lub Lua) mogą zapewnić większą wygodę dla konkretnych zastosowań, balansując między łatwością użycia a potrzebami wydajności.
