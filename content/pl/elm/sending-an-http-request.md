---
title:                "Wysyłanie żądania http"
html_title:           "Elm: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach wiele aplikacji internetowych wymaga komunikacji z zewnętrznymi serwerami. W celu uzyskania danych lub wykonania jakiejś akcji, potrzebne jest wysłanie żądania HTTP. W takim przypadku, znajomość sposobów wysyłania żądań HTTP jest niezbędna dla każdego programisty Elm, który chce tworzyć aplikacje internetowe.

## Jak to zrobić

W Elm do wykonania żądania HTTP służy funkcja `Http.send`. Przyjmuje ona dwa argumenty - rodzaj żądania oraz adres URL, na który ma zostać wysłane żądanie.

```Elm
-- Przykładowe żądanie GET
Http.send Get "https://example.com/api/data"
```

W odpowiedzi na żądanie, zostanie wykonany efekt uboczny, który można obsłużyć wewnątrz funkcji `update`. Przykładowo, w przypadku poprawnego żądania GET, otrzymamy odpowiedź zawierającą dane w formacie JSON.

```Elm
-- Przykładowe obsługiwanie odpowiedzi
type Msg
    = DataReceived (Result Http.Error Data)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        DataReceived result ->
            case result of
                Ok data ->
                    -- Obsługa danych
                Err error ->
                    -- Obsługa błędu
```

Aby móc używać funkcji `Http.send`, należy zaimportować moduł `Http`. Wpraktyce, warto również użyć paczki `elm-tasks-http` do wygodniejszego zarządzania zadaniami HTTP.

## Głębsze zagłębienie

Funkcje `Http.send` korzystają z mechanizmu efektów ubocznych w Elm. Oznacza to, że funkcja może wykonać jakieś zadanie i zwrócić efekt uboczny, który będzie obsłużony w funkcji `update`. Dzięki temu, aplikacja nie zatrzymuje swojego działania na czas wykonywania żądania HTTP.

Ponadto, istnieje możliwość dostosowania żądania poprzez ustawienie nagłówków oraz ciała żądania. Możliwe jest również przetwarzanie odpowiedzi w innym formacie, np. binarnym zamiast JSON.

Więcej informacji na temat wysyłania żądań HTTP w Elm można znaleźć w dokumentacji [książki Elm](https://guide.elm-lang.org/effects/http.html) oraz w repozytorium paczki `elm-tasks-http`.

## Zobacz również

- [Paczka elm-tasks-http](https://github.com/elm-tasks/http)
- [Dokumentacja Elm - Wysyłanie żądań HTTP](https://guide.elm-lang.org/effects/http.html)