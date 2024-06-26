---
date: 2024-01-26 03:49:16.521767-07:00
description: "Jak to zrobi\u0107: Elm nie posiada wbudowanego debugera w tradycyjnym\
  \ sensie, w jaki ma to miejsce np. w JavaScript z narz\u0119dziami deweloperskimi\
  \ przegl\u0105darki.\u2026"
lastmod: '2024-03-13T22:44:35.326244-06:00'
model: gpt-4-0125-preview
summary: Elm nie posiada wbudowanego debugera w tradycyjnym sensie, w jaki ma to miejsce
  np.
title: Korzystanie z debugera
weight: 35
---

## Jak to zrobić:
Elm nie posiada wbudowanego debugera w tradycyjnym sensie, w jaki ma to miejsce np. w JavaScript z narzędziami deweloperskimi przeglądarki. Jednakże społeczność Elm stworzyła narzędzia wypełniające tę lukę. Oto jak możesz użyć `elm-debug-transformer` do debugowania twojej aplikacji Elm:

```Elm
-- Zainstaluj elm-debug-transformer (pakiet Node)

1. npm install -g elm-debug-transformer

-- Użyj elm-debug-transformer do uruchomienia twojej aplikacji

2. elm-debug-transformer --port=8000 yourMainElmFile.elm 
```

Gdy `elm-debug-transformer` jest uruchomiony, tworzy połączenie WebSocket do logowania. Zobaczysz informacje dotyczące debugowania w konsoli twojej przeglądarki, gdzie możesz inspekcjonować struktury danych twojego programu w danych punktach twojej aplikacji.

W Elm 0.19 i późniejszych, funkcje modułu `Debug` takie jak `Debug.log` i `Debug.todo` mogą pomóc ci śledzić wartości i celowo oznaczać nieukończone części twojego kodu. Oto jak używać Debug.log:

```Elm
import Debug

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( Debug.log "Zwiększam" { model | count = model.count + 1 }, Cmd.none )

        Decrement ->
            ( Debug.log "Zmniejszam" { model | count = model.count - 1 }, Cmd.none )
```

Zobaczysz w konsoli twojej przeglądarki wiadomości "Zwiększam" lub "Zmniejszam" wraz z nowym stanem `modelu`.

## Dogłębna analiza
Autor Elm, Evan Czaplicki, miał na celu stworzenie języka, w którym częste błędy byłyby niemożliwe do popełnienia lub łatwe do wykrycia. Ta filozofia jest powodem, dla którego rdzeń Elm nie zawiera tradycyjnych funkcji debugowania. Statyczna analiza i wnioskowanie typów w Elm znacząco przyczyniają się do redukcji błędów w czasie wykonania, co zmniejsza potrzebę zaawansowanego debugowania w czasie wykonania. Historyczne alternatywy obejmowały użycie obecnie przestarzałego `elm-reactor`, który oferował debugowanie z cofaniem czasu – sposób na przewijanie wstecz i odtwarzanie akcji w twojej aplikacji.

Dziś narzędzia takie jak `elm-debug-transformer` i użycie modułu `Debug` Elm pomagają zapełnić tę lukę. Chociaż moduł `Debug` jest przeznaczony do użycia tylko w trakcie rozwoju i powinien zostać usunięty przed budowaniem wersji produkcyjnych, jest nieocenionym narzędziem do wskazywania i logowania zmian stanu.

Należy pamiętać, że tradycyjne techniki debugowania JavaScript, takie jak punkty przerwania czy wykonanie krok po kroku, nie są bezpośrednio stosowalne w Elm ze względu na jego architekturę i obsługę aktualizacji stanu przez środowisko wykonawcze Elm. Elm zachęca do strukturowania programu w taki sposób, aby przepływ danych był jasny i zgodny z rygorystycznymi typami oraz gwarancjami niemutowalności, minimalizując przypadki, gdy potrzebne jest debugowanie.

## Zobacz też
- Oficjalny przewodnik Elm dotyczący obsługi wyjątków w czasie wykonania: https://guide.elm-lang.org/error_handling/
- Repozytorium GitHub `elm-debug-transformer`: https://github.com/kraklin/elm-debug-transformer
- Wątek dyskusyjny Elm dotyczący strategii debugowania: https://discourse.elm-lang.org/c/show-and-tell/debugging
- Dokumentacja modułu `Debug` Elm: https://package.elm-lang.org/packages/elm/core/latest/Debug
