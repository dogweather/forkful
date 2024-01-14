---
title:                "Elm: Generowanie losowych liczb"
programming_language: "Elm"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie liczb losowych jest ważną częścią wielu programów, szczególnie tych związanych z grami, symulacjami czy testowaniem. Przypomnijmy sobie, jak ważne jest, aby każda gra lub plansza były unikalne i różniły się od siebie w celu zapewnienia użytkownikom ciekawych i zróżnicowanych doświadczeń. W tym wpisie opowiemy o sposobach generowania liczb losowych w języku Elm.

## Jak

### Generowanie liczb losowych w zakresie

Aby wygenerować liczbę losową w określonym zakresie, musimy użyć funkcji `Random.int` wraz z odpowiednimi parametrami. Przykładowo, jeśli chcemy wylosować liczbę z zakresu od 1 do 10, nasz kod będzie wyglądał następująco:

```Elm
import Random

Random.int 1 10
```

Powyższy przykład zwróci nam wylosowaną liczbę, ale co jeśli chcemy wyświetlić ją użytkownikowi? W tym celu możemy wykorzystać funkcję `Random.Generate`, która pozwoli nam wygenerować wartość i przekazać ją do wybranej funkcji. Przykładowo, aby wyświetlić wylosowaną liczbę na stronie, możemy użyć funkcji `Html.text`:

```Elm
import Html exposing (text)
import Random

Random.generate GetRandomNumber (Random.int 1 10)

type Msg = GetRandomNumber Int

update msg model =
    case msg of
        GetRandomNumber randomNum ->
            { model | randomNumber = randomNum }

view model =
    text (toString model.randomNumber)
```

### Generowanie liczb losowych na podstawie listy

Czasami potrzebujemy wygenerować wartość losową na podstawie wcześniej zdefiniowanej listy. W takim przypadku możemy użyć funkcji `Random.shuffled` wraz z naszą listą jako parametrem. Przykładowo, mamy listę imion użytkowników i chcemy wylosować z niej jedno imię:

```Elm
import Random
import Html exposing (text)

userNames = ["Anna", "Jan", "Kasia", "Piotr", "Maria"]

Random.shuffled userNames
    |> Random.generate GetRandomName

type Msg = GetRandomName String

update msg model =
    case msg of
        GetRandomName randomName ->
            { model | randomName = randomName }

view model =
    text model.randomName
```

## Deep Dive

Generowanie liczb losowych w Elm odbywa się przy użyciu funkcji `Random`, która implementuje algorytm Mersenne Twister. Jest to bardzo popularny i sprawdzony sposób na generowanie liczb pseudolosowych, który zapewnia wyższą jakość generowanych wartości.

W języku Elm istnieją także inne funkcje pozwalające na bardziej precyzyjną kontrolę nad generowanymi liczbami, takie jak `Random.float` czy `Random.bool`. Odpowiednie wykorzystanie tych funkcji może pomóc w tworzeniu bardziej zaawansowanych programów i gier.

## Zobacz także

- Dokumentacja języka Elm: https://elm-lang.org/docs
- Przewodnik po generatorze liczb losowych: https://guide.elm-lang.org/effects/random.html
- Przykładowe implementacje w języku Elm: https://github.com/mweststrate/elm-random-extra