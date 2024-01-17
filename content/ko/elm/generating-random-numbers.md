---
title:                "난수 생성하기"
html_title:           "Elm: 난수 생성하기"
simple_title:         "난수 생성하기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

무작위 숫자 생성은 숫자를 무작위로 생성하는 것을 말합니다. 이는 프로그래머들이 랜덤한 데이터를 생성하여 다양한 시나리오를 시뮬레이션하고 테스트하는데 유용합니다.

## 무엇 & 왜?

무작위 숫자 생성은 매우 간단하고 기본적인 프로그래밍 작업입니다. 랜덤한 데이터를 사용하여 프로그램의 여러 가지 경우를 시뮬레이션하거나 데이터를 테스트하는 데 사용됩니다. 예를 들어, 게임에서 적의 위치를 랜덤하게 생성하거나, 인터넷 검색 결과를 섞어서 보여주는 등 다양한 용도로 사용될 수 있습니다.

## 방법:

Elm에서는 Random 라이브러리를 사용하여 무작위 숫자를 생성할 수 있습니다. 아래의 코드를 참고하여 간단한 예제를 살펴보세요.

```
module Main exposing (main)

import Random exposing (..)

type Msg = 
    NewRandomNumber Int

update : Msg -> Int -> (Int, Cmd Msg)
update msg currentNumber =
    case msg of
        NewRandomNumber num ->
            (num, Cmd.none)

randomNumber : Random.Generator Int
randomNumber =
    Random.int 1 10 -- 1부터 10 사이의 랜덤한 정수 생성

main : Program Int
main =
    Html.beginnerProgram
        { model = 0
        , view = view
        , update = update
        }

view : Int -> Html Msg
view number =
    Html.div []
        [ Html.text ("Random number: " ++ (toString number)) ]

subscriptions : Int -> Sub Msg
subscriptions num =
    Sub.none

```

### 출력:

```
Random number: 7
```

## 조금 더 깊이 들어가보기:

무작위 숫자 생성은 컴퓨터 과학에서 매우 중요한 개념입니다. 이는 알고리즘에서 필수적인 요소로 사용되며, 확률 분포에 따라 랜덤한 데이터를 생성하는 다양한 방법들이 존재합니다. 한 가지 대안으로는 수학적으로 랜덤한 난수를 생성하는 알고리즘을 사용하는 것이 있는데, 이는 무작위성을 위해 물리적인 장치를 사용하지 않아도 된다는 장점이 있습니다. Elm의 Random 라이브러리는 우리가 원하는 확률 분포를 정확하게 조절할 수 있도록 다양한 함수들을 제공합니다.

## 더 알아보기:

- [Elm Random 라이브러리 문서](https://package.elm-lang.org/packages/elm/random/latest/Random)
- [랜덤 함수와 무작위성에 대한 주의](https://elmprogramming.com/randomness-with-elm.html)