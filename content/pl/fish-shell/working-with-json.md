---
title:                "Fish Shell: Praca z json"
simple_title:         "Praca z json"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

## Dlaczego warto pracować z JSON?

JSON (JavaScript Object Notation) jest popularnym formatem przechowywania i przesyłania danych w aplikacjach internetowych. Dzięki swojej prostocie i czytelności, JSON jest często wykorzystywany w tworzeniu interfejsów programistycznych (API). Praca z JSON może być niezbędna dla programistów, którzy chcą korzystać z dostępnych danych ze stron internetowych lub tworzyć własne aplikacje.

## Jak to zrobić?

Kodowanie w Fish Shell z użyciem JSON jest bardzo proste i wygodne. Do tego celu możemy wykorzystać wbudowane polecenia i funkcje Fish Shell. Na przykład, aby odczytać dane z pliku JSON, możemy użyć polecenia ```cat```:

```Fish Shell
cat example.json
```

Pozwoli nam to wyświetlić zawartość pliku JSON w konsoli. Natomiast, aby przetworzyć dane z pliku i wyświetlić je w formacie JSON, możemy użyć funkcji ```json_pp```:

```Fish Shell
cat example.json | json_pp
```

Dzięki temu funkcja przetworzy dane i wyświetli je w czytelnej formie w konsoli. Jeśli chcemy zapisywać dane w formacie JSON, możemy skorzystać z funkcji ```json_out```:

```Fish Shell
echo '{ "id": 123, "name": "John", "age": 30 }' | json_out > output.json
```

W ten sposób zapiszemy dane z konsoli do pliku w formacie JSON.

## Głębsza analiza

Obsługa danych JSON w Fish Shell jest jeszcze bardziej rozbudowana, dzięki różnym funkcjom i poleceniom dostępnym w Shellu. Możemy na przykład użyć funkcji ```json_escape``` do zabezpieczania danych przed niebezpiecznymi znakami lub użyć polecenia ```jq``` do filtrowania i przetwarzania danych w formacie JSON. W Fish Shell istnieje także możliwość wykorzystywania zmiennych i pętli w celu bardziej zaawansowanej pracy z JSON.

## Zobacz także

- [Oficjalna dokumentacja Fish Shell](https://fishshell.com/docs/current/index.html)
- [Format JSON na stronie W3Schools](https://www.w3schools.com/js/js_json_intro.asp)
- [Kurs programowania w Fish Shell](https://www.digitalocean.com/community/tutorial_series/getting-started-with-fish-shell)