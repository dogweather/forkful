---
title:    "Elm: Generowanie losowych liczb"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego?

Generowanie losowych liczb jest niezbędnym elementem wielu aplikacji i programów. W Elm mamy wiele sposobów na wykorzystanie generatorów liczb losowych, co pozwala na stworzenie ciekawych i interaktywnych aplikacji.

## Jak to zrobić?

Pierwszym krokiem jest importowanie pakietu `Random` w naszym kodzie Elm:

```elm
import Random
```

Następnie, możemy stworzyć generator, który będzie zwracał losową liczbę z zakresu od 0 do 10:

```elm
Random.int 0 10
```

Aby uzyskać konkretny wynik, musimy przypisać wygenerowany generator do zmiennej i użyć funkcji `generate`:

```elm
randomNumber = Random.generate Random.int 0 10
```

Kolejnym krokiem jest obsłużenie wyniku w funkcji `Html.program`:

```elm
Html.program {
    init = init,
    update = update,
    view = view,
    subscriptions = subscriptions,
    model = 0
}
```

W funkcji `view` możemy wyświetlić wygenerowaną liczbę:

```elm
view : Int -> Html msg
view number = 
    Html.text (toString number)
```

Po uruchomieniu naszej aplikacji, za każdym razem gdy odświeżymy stronę, zostanie wyświetlona losowa liczba z zakresu od 0 do 10.

## Głębsze zagadnienia

Podczas korzystania z generatorów liczb losowych, warto mieć na uwadze kilka ważnych aspektów. Pierwszym z nich jest kontrolowanie ziarna generatora, czyli początkowego stanu. Dzięki temu możemy zapewnić, że za każdym razem otrzymamy inne wyniki. Możemy to zrobić przy użyciu funkcji `initialSeed` lub `initialSeedWith` z modułu `Random.Seed`.

Kolejnym ważnym elementem jest wykorzystanie generatorów w celu generowania losowych elementów albo w sposób ciągły albo w reakcji na interakcje użytkownika. Aby to osiągnąć, możemy użyć funkcji `step` wraz z aktualnym stanem generatora.

## Zobacz także

- [Dokumentacja Elm - Random](https://package.elm-lang.org/packages/elm/random/latest/)
- [Przykładowy projekt w Elm wykorzystujący generatory liczb losowych](https://github.com/ksolitary/elm-random-number-generator)
- [Przykłady użycia generatorów liczb losowych w aplikacjach Elm](https://medium.com/@ksolitary/using-random-number-generators-in-elm-6775c702e680)