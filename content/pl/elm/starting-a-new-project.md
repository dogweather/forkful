---
title:                "Rozpoczynanie nowego projektu"
html_title:           "Bash: Rozpoczynanie nowego projektu"
simple_title:         "Rozpoczynanie nowego projektu"
programming_language: "Elm"
category:             "Elm"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Rozpoczęcie nowego projektu to proces tworzenia od podstaw skomplikowanej aplikacji lub strony internetowej w języku Elm. Programiści robią to, aby zrozumieć i wykorzystać w pełni możliwości tego języka, a także stworzyć rozwiązania spełniające konkretne wymagania klientów.

## Jak to zrobić:

Do rozpoczęcia nowego projektu w Elm używamy narzędzia `elm-init`. Oto przykład kodu i wyniku:

```Elm
cd ~/MojeProjekty
elm init
```

Twoje wyjście będzie wyglądać mniej więcej tak:

```
Here is my plan:

    Initialize a new Git repo in ~/MojeProjekty?
    │
    └─> Yes
        │
        └─> Ok. I created an "elm.json" file for you.
```

## Pogłębione spojrzenie:

Rozpoczęcie projektu w Elm to nie tylko pisanie kodu, ale także zrozumienie, jak język funkcjonuje. Elm wywodzi się z języków funkcyjnych, takich jak Haskell, i jest przeznaczony do tworzenia bezpiecznych, wydajnych aplikacji internetowych. 

Jednym z alternatywnych podejść jest użycie frameworków JavaScript, takich jak React czy Angular. Elm jest jednak znacznie bardziej bezpieczny, łatwiej przewiduje błędy na etapie kompilacji i zapewnia lepszą wydajność.

Szczegółem implementacyjnym, który warto rozważyć, jest to, że Elm kompiluje cały swój kod do JavaScriptu, co umożliwia łatwe wdrożenie go na dowolnej stronie internetowej. Elm ma również własne narzędzia, takie jak `elm reactor` i `elm make`, które pomagają w procesie tworzenia kodu.

## Zobacz też:

- Oficjalna dokumentacja Elm: https://elm-lang.org/docs
- Przewodnik po Elm: https://guide.elm-lang.org/
- Repozytorium Elm na GitHub: https://github.com/elm/elm-lang.org
- Kurs Elm na freeCodeCamp: https://www.freecodecamp.org/news/an-intro-to-elm-programming-language/