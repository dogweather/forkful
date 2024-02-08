---
title:                "Refaktoryzacja"
date:                  2024-01-26T01:17:55.596907-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refaktoryzacja"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/refactoring.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Refaktoryzacja to w zasadzie wiosenne sprzątanie w Twojej bazie kodu - chodzi o restrukturyzację istniejącego kodu bez zmiany jego zewnętrznego zachowania. Programiści robią to, aby kod był czytelniejszy, zredukować złożoność, poprawić możliwości utrzymania oraz aby łatwiej go było rozszerzać.

## Jak to zrobić:
Załóżmy, że masz funkcję Elm, która robi za dużo, jak mieszanie logiki UI z aktualizacjami stanu. To idealny kandydat do refaktoryzacji. Oryginalnie:

```Elm
updateAndFormat : String -> Model -> (Model, Cmd Msg)
updateAndFormat input model =
    let
        updatedModel = { model | userInput = input }
    in
    if String.length input > 5 then
        ( updatedModel, Cmd.none )
    else
        ( model, Cmd.none )
```

Po refaktoryzacji, rozdzielamy troski poprzez wyjęcie logiki do różnych funkcji:

```Elm
-- Logika aktualizacji jest oddzielona
updateUserInput : String -> Model -> Model
updateUserInput input model = 
    { model | userInput = input }

-- Logika formatowania (widoku) jest także oddzielona
formatUserInput : Model -> (Model, Cmd Msg)
formatUserInput model =
    if String.length model.userInput > 5 then
        ( model, Cmd.none )
    else
        ( { model | userInput = "" }, Cmd.none ) -- Czyść wprowadzanie, jeśli jest za krótkie, jako przykładowa reguła.

-- Funkcja aktualizacji teraz używa funkcji pomocniczych
updateAndFormat : String -> Model -> (Model, Cmd Msg)
updateAndFormat input model =
    model
    |> updateUserInput input
    |> formatUserInput
```
Z tymi zmianami, masz jasne oddzielenie, a każda funkcja jest łatwiejsza do zrozumienia i testowania.

## Dogłębna analiza
Refaktoryzację jako formalną praktykę można prześledzić aż do wczesnych dni programowania, kiedy to koszt zmiany kodu był już rozpoznany jako krytyczny aspekt procesu rozwoju. Godnym uwagi jest, że książka Martina Fowlera "Refaktoryzacja: Ulepszanie struktury istniejącego kodu," opublikowana pod koniec lat 90-tych, naprawdę ustawiła scenę dla refaktoryzacji z zorganizowanym podejściem i katalogiem "zapachów kodu", aby identyfikować okazje do refaktoryzacji.

W kontekście Elm, refaktoryzacja wykorzystuje mocne strony języka, takie jak jego silny system typów, który promuje zaufanie podczas procesu. Alternatywy dla ręcznej refaktoryzacji mogą obejmować zautomatyzowane narzędzia transformacji kodu, ale narzędzia Elm w tym obszarze są wciąż na etapie dojrzewania w porównaniu do niektórych starszych języków. Szczegóły implementacji często dotyczą typowych refaktoryzacji, takich jak ekstrakcja funkcji, przemianowanie i upraszczanie warunków. Kompilator Elm jest kluczowym sojusznikiem w refaktoryzacji, ponieważ nie pozwoli Ci wiele przeskoczyć - krzyczy zawsze, gdy coś jest nie tak, zapewniając, że Twój zrefaktoryzowany kod nadal działa.

## Zobacz również
- ["Refaktoryzacja: Ulepszanie struktury istniejącego kodu" autorstwa Martina Fowlera](https://martinfowler.com/books/refactoring.html)
- [Elm Discourse - Tematy o refaktoryzacji](https://discourse.elm-lang.org/search?q=refactoring)
