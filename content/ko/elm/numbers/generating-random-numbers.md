---
changelog:
- 2024-02-27, dogweather, edited and tested
- 2024-02-27, gpt-4-0125-preview, translated from English
date: 2024-02-27 22:50:37.211859-07:00
description: "\uBC29\uBC95: Elm\uC758 \uC21C\uC218 \uD568\uC218\uC801 \uD2B9\uC131\
  \uC740 \uBA85\uB839\uD615 \uC5B8\uC5B4\uC5D0\uC11C\uCC98\uB7FC \uC9C1\uC811 \uBB34\
  \uC791\uC704 \uC218\uB97C \uC0DD\uC131\uD560 \uC218 \uC5C6\uB2E4\uB294 \uAC83\uC744\
  \ \uC758\uBBF8\uD569\uB2C8\uB2E4. \uB300\uC2E0, `Random` \uBAA8\uB4C8\uC744 \uBA85\
  \uB839\uACFC \uD568\uAED8 \uC0AC\uC6A9\uD569\uB2C8\uB2E4. 1\uBD80\uD130 100 \uC0AC\
  \uC774\uC758 \uBB34\uC791\uC704 \uC815\uC218\uB97C \uC0DD\uC131\uD558\uB294 \uAE30\
  \uBCF8 \uC608\uAC00 \uC5EC\uAE30 \uC788\uC2B5\uB2C8\uB2E4. \uC6B0\uC120, `elm install\u2026"
lastmod: '2024-03-13T22:44:55.104812-06:00'
model: gpt-4-0125-preview
summary: "Elm\uC758 \uC21C\uC218 \uD568\uC218\uC801 \uD2B9\uC131\uC740 \uBA85\uB839\
  \uD615 \uC5B8\uC5B4\uC5D0\uC11C\uCC98\uB7FC \uC9C1\uC811 \uBB34\uC791\uC704 \uC218\
  \uB97C \uC0DD\uC131\uD560 \uC218 \uC5C6\uB2E4\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\
  \uB2C8\uB2E4."
title: "\uB79C\uB364 \uC22B\uC790 \uC0DD\uC131\uD558\uAE30"
weight: 12
---

## 방법:
Elm의 순수 함수적 특성은 명령형 언어에서처럼 직접 무작위 수를 생성할 수 없다는 것을 의미합니다. 대신, `Random` 모듈을 명령과 함께 사용합니다. 1부터 100 사이의 무작위 정수를 생성하는 기본 예가 여기 있습니다.

우선, `elm install elm/random`로 `Random` 모듈을 설치합니다. 그런 다음 필요한 HTML 및 이벤트 모듈과 함께 Elm 파일에 가져옵니다. 다음과 같습니다:

`src/Main.elm`

```elm
module Main exposing (..)

import Browser
import Html exposing (Html, button, text, div)
import Html.Events exposing (onClick)
import Random
```

이 예제를 독립 실행 형식으로 만들기 위해서는 이런 템플릿을 추가할 수 있습니다:
```elm
main =
  Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }

init : () -> (Model, Cmd Msg)
init _ =
  (Model 0, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none
```

다음으로, 무작위 수를 생성할 **명령**을 정의합니다. 이는 생성될 무작위 수를 처리하는 `Msg` 유형과, 이를 저장할 `Model`, 그리고 이 모든 것을 함께 묶는 업데이트 함수를 설정하는 것을 포함합니다.
```elm
type Msg
    = Generate
    | NewRandom Int

type alias Model = { randomNumber : Int }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Generate ->
            ( model, Random.generate NewRandom (Random.int 1 100) )

        NewRandom number ->
            ( { model | randomNumber = number }, Cmd.none )
```

무작위 수 생성을 트리거하려면, 예를 들어, 뷰에서 `Generate` 메시지를 보내는 버튼을 통해 이를 수행할 수 있습니다:
```elm
view : Model -> Html Msg
view model =
    div []
        [ div [] [ text ("Random Number: " ++ String.fromInt model.randomNumber) ]
        , button [ onClick Generate ] [ text "Generate" ]
        ]
```

"Generate" 버튼을 클릭하면 1부터 100 사이의 무작위 수가 표시됩니다.

이 단순한 접근 방식은 적응하고 확장될 수 있으며, `Random` 모듈에 있는 다른 함수를 사용하여 무작위 부동소수점, 리스트 또는 사용자 정의 유형에 기반한 복잡한 데이터 구조까지 생성할 수 있어 Elm 애플리케이션에 예측 불가능성을 더하는 광범위한 장을 제공합니다.

Elm 가이드에서는 훨씬 더 자세히 설명하고 있습니다. 또한 [여섯 면체 주사위를 굴리는 예제](https://guide.elm-lang.org/effects/random)도 있습니다.
