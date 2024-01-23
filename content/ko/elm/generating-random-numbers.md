---
title:                "난수 생성하기"
date:                  2024-01-20T17:49:04.513254-07:00
model:                 gpt-4-1106-preview
simple_title:         "난수 생성하기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (무엇 그리고 왜?)

랜덤 숫자 생성은 예측할 수 없는 숫자를 만드는 기법입니다. 프로그래머들은 게임, 시뮬레이션, 보안 알고리즘 등에 다양하고 무작위적인 결과를 보장하기 위해 이를 사용합니다.

## How to: (하는 방법:)

Elm에서 랜덤 숫자를 생성하기 위해 `Random` 모듈을 사용합니다. 아래는 1에서 100 사이의 랜덤 숫자를 생성하는 간단한 예제입니다.

```Elm
import Random

-- 랜덤 숫자 생성기를 초기화합니다
randomGenerator : Random.Generator Int
randomGenerator = Random.int 1 100

-- 프로그램 상태를 나타내는 모델입니다
type alias Model = Int

-- 초기 모델
init : (Model, Cmd Msg)
init = (0, Random.generate NewRandomNumber randomGenerator)

-- 메시지 타입을 정의합니다
type Msg = NewRandomNumber Int

-- 뷰를 정의하지 않고, 돔에 그리는 함수도 생략합니다
-- ...

-- `Update` 함수에서 랜덤 메시지를 처리합니다
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NewRandomNumber number ->
            (number, Cmd.none)
```

랜덤 숫자는 ‘init’ 함수에서 프로그램이 시작될 때 생성되며, 새로운 숫자는 `NewRandomNumber` 메시지를 통해 ‘update’ 함수로 전달됩니다.

## Deep Dive (깊이 알아보기)

Elm의 랜덤 숫자 생성은 순수 함수형 언어의 특성 상 가변 상태를 피합니다. 이는 '시드'를 사용하여 재현 가능한 방식으로 랜덤성을 생성합니다. 예전의 함수형 언어들도 비슷한 접근 방식을 취했었죠.

다른 언어들에서는 종종 내장된 글로벌 랜덤 함수를 볼 수 있지만, Elm에서는 명시적으로 랜덤 시퀀스를 제어해야 합니다. 이는 예측 가능한 테스트와 유지보수에도 도움이 됩니다.

Elm에서는 `Random.step` 함수를 사용하여 시드를 업데이트하고 다음 랜덤 값을 얻을 수 있는데, 이는 오류 가능성을 줄이고 함수의 순수성을 유지하는데 중요합니다.

## See Also (함께 보기)

- Elm 공식 문서의 랜덤 모듈 설명: [Random - Elm Package](https://package.elm-lang.org/packages/elm/random/latest/)
- Elm 안내서의 예제들: [Elm Guide - Random](https://guide.elm-lang.org/effects/random.html)
