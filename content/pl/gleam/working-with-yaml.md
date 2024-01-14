---
title:                "Gleam: Praca z yaml"
simple_title:         "Praca z yaml"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

## Dlaczego

YAML jest formatem plików konfiguracyjnych, który jest bardzo przydatny w programowaniu. Dzięki jasnej i czytelnej składni, jest łatwy w użyciu nawet dla początkujących programistów. Pozwala na definiowanie danych w sposób hierarchiczny, co ułatwia organizowanie dużych struktur danych.

## Jak to zrobić

```Gleam
type Config {
    database: String,
    port: Int,
    debug: Bool,
    servers: [String],
    colors: map(String, String)
}

const config = from_yaml(result)
match config {
    Ok(config) ->
        servers = config.servers
        colors = config.colors
        Gleam_io.Console.log("Database: " ++ config.database)
        Gleam_io.Console.log("Port: " ++ config.port)
        Gleam_io.Console.log("Debug mode: " ++ config.debug)
        Gleam_io.Console.log("Servers: " ++ servers)
        Gleam_io.Console.log("Colors: " ++ colors)
    Err(error) ->
        Gleam_io.Console.log("Unable to read YAML file: " ++ error)
}
```

Ten przykład pokazuje, w jaki sposób można wczytać dane z pliku YAML i korzystać z informacji w nim zawartych. Dzięki funkcji `from_yaml` możemy łatwo przekonwertować plik YAML na strukturę danych w Gleamie i następnie odwoływać się do jej pól. Konieczne jest jednak zadbanie o obsługę błędów, gdy plik będzie w niepoprawnym formacie lub nie będzie istnieć.

## Głębszy zanurz

Podczas pracy z YAML warto zwrócić uwagę na kilka ważnych kwestii. Po pierwsze, format ten jest czuły na wcięcia, więc musimy upewnić się, że wszystkie elementy w danym bloku mają taką samą liczbę spacji przed sobą. Po drugie, za pomocą znaku `#` możemy w komentarzach zawrzeć informacje pomocne przy rozwoju lub dokumentacji naszego kodu. Ponadto, warto pamiętać o różnych typach danych, jakie można przypisać do pól w pliku YAML - od prostych stringów i liczb po bardziej złożone struktury danych, jak mapy czy listy.

## Zobacz również

* [Dokumentacja języka Gleam](https://gleam.run)
* [Dokumentacja formatu YAML](https://yaml.org)
* [Przykład aplikacji Gleam z wykorzystaniem YAML](https://github.com/exampleapp)