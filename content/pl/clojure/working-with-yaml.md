---
title:                "Pracując z yaml"
html_title:           "Clojure: Pracując z yaml"
simple_title:         "Pracując z yaml"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Praca z YAML jest niezwykle ważna dla programistów ze względu na to, że jest to format przechowywania danych, który jest czytelny zarówno dla ludzi, jak i dla maszyn. Dzięki YAML możemy łatwo przesyłać i odczytywać struktury danych, co ułatwia nam pracę z aplikacjami i kodem.

## Jak:

```Clojure
;; Przykładowe dane w formacie YAML
{:imie "Jan"
 :nazwisko "Kowalski"
 :wiek 35
 :miasto "Warszawa"}
```

```Clojure
;; Załadowanie danych z pliku YAML przy użyciu biblioteki
(clojure-yaml/read "dane.yaml")
```

## Głębsze zagadnienia:

W YAML możemy przechowywać różnego rodzaju dane, od tekstów i liczb po bardziej skomplikowane struktury. Format ten został stworzony przez Clarka Evansa w 2001 roku i jest często wykorzystywany w prostych bazach danych. Alternatywą dla YAML jest na przykład JSON lub XML.

Implementacja YAML w języku Clojure odbywa się dzięki bibliotece clojure-yaml, która umożliwia łatwe odczytywanie i zapisywanie danych w tym formacie. Dzięki niej możemy integrować nasze aplikacje z innymi systemami wykorzystującymi YAML.

## Zobacz też:

- [Oficjalna strona formatu YAML](https://yaml.org)