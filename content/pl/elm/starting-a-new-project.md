---
title:                "Rozpoczynanie nowego projektu"
html_title:           "Elm: Rozpoczynanie nowego projektu"
simple_title:         "Rozpoczynanie nowego projektu"
programming_language: "Elm"
category:             "Elm"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli planujesz rozpocząć nowy projekt w języku Elm, powinieneś wiedzieć, że jest to jeden z najnowszych i najbardziej nowoczesnych języków programowania. Zastosowanie Elm w swoim projekcie pozwoli Ci na tworzenie skalowalnych aplikacji internetowych, które są szybkie, bezpieczne i wydajne.  

## Jak to zrobić 

### Instalacja Elm

Przed rozpoczęciem pracy z Elm, musisz go zainstalować. Najprostszym sposobem jest pobranie i zainstalowanie Elm Platform ze strony https://guide.elm-lang.org/install.html (obsługuje ona Windows, Mac oraz Linux).

### Podstawy Elm

Po zainstalowaniu Elm możesz rozpocząć pracę nad swoim projektem. Ogólnie rzecz biorąc, Elm jest językiem funkcyjnym, co oznacza, że wszystko w nim jest funkcją. Jednaj musisz się przyzwyczaić do tego sposobu myślenia, aby pisać w nim efektywny i elegancki kod.

Załóżmy, że chcesz napisać prostą aplikację, która przyjmuje od użytkownika imię i wyświetla powitanie. Poniżej znajduje się przykładowy kod:

```Elm
import Html exposing (..)
import Html.Attributes exposing (..)

type alias Model = 
    { name: String
    }

view model = 
    div [] 
        [ h1 [] [ text ("Witaj " ++ model.name ++ "!") ]
        , input [ type_ "text", placeholder "Wpisz swoje imię", onInput ChangeName ] []
        ]

ChangeName userInput = 
    { model | name <- userInput }

main =
    beginnerProgram
        { model = { name = "" }, view = view, update = update }
```

Po uruchomieniu kodu, zobaczysz proste okienko z polem tekstowym i powitaniem. Wpisanie imienia w polu spowoduje zmianę powitania w nagłówku. 

### Zdarzenia 

Elm obsługuje również zdarzenia, które pozwalają na interakcję użytkownika z aplikacją. W naszym przykładzie, kiedy użytkownik wpisze swoje imię i naciśnie enter, chcemy, aby aplikacja wyświetliła podziękowanie. Aby to osiągnąć, należy dodać funkcję obsługującą zdarzenie w naszym kodzie:

```Elm
import Html.Events exposing (onEnter)

view model = 
    div [] 
        [ h1 [] [ text ("Witaj " ++ model.name ++ "!") ]
        , input [ type_ "text", placeholder "Wpisz swoje imię", onEnter EnterName ] []
        ]

EnterName userInput = 
    [ h2 [] [ text ("Dziękujemy za wpisanie imienia, " ++ userInput ++ "!") ]
    , { model | name <- userInput }
    ]
```

Po wpisaniu imienia i naciśnięciu enter, zobaczysz nowy nagłówek z podziękowaniem.

## Głębsza analiza 

Podczas tworzenia nowego projektu warto zapoznać się z dokumentacją Elm, gdzie znajdują się szczegóły na temat tego języka oraz wiele przydatnych przykładów. 

Jeśli chcesz zwiększyć zakres swojej wiedzy na temat Elm, polecam także kursy online, takie jak "Elm for Beginners" czy "Advanced Elm". Są one bogate w praktyczne przykłady i w rzetelny sposób wprowadzą Cię w świat tego języka programowania.

## Zobacz także 

- [Oficjalna strona Elm](https://elm-lang.org/)
- [Dokumentacja Elm](https://guide.elm-lang.org/)
- [Kurs "Elm for Beginners"](https://courses.knowthen.com/p/elm-for-beginners)
- [Kurs "Advanced Elm"](https://courses.knowthen.com/p/advanced-elm)