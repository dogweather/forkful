---
title:                "난수 생성"
aliases: - /ko/elm/generating-random-numbers.md
date:                  2024-01-27T20:33:49.984776-07:00
model:                 gpt-4-0125-preview
simple_title:         "난수 생성"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 무엇이며 왜인가?
Elm에서 난수를 생성하는 것은 게임, 시뮬레이션 및 보안 알고리즘과 같은 애플리케이션에 필수적인 예측할 수 없는 숫자 값을 만드는 과정을 포함합니다. 프로그래머들은 실제 세계의 변동성을 시뮬레이션하고, 사용자 경험을 향상시키거나, 암호화 기술로 데이터를 보안하는 데 난수를 사용합니다.

## 방법:
Elm은 많은 프로그래밍 언어와 달리, 함수를 순수하게 유지하는 시스템을 사용하여 난수를 다룹니다. 난수를 생성하려면 Elm의 `Random` 모듈로 작업해야 합니다. 1부터 100 사이의 난수를 생성하는 기본 예제는 다음과 같습니다:

```Elm
import Html exposing (Html, text)
import Random

main : Html msg
main =
    Random.generate NewRandomNumber (Random.int 1 100)
    |> Html.map (text << toString)

type Msg = NewRandomNumber Int
```

이 코드 조각은 지정된 범위 내에서 난수를 생성하는 명령을 만들기 위해 `Random.generate`를 사용합니다. `type Msg` 선언은 Elm 애플리케이션의 업데이트 함수에서 생성된 숫자를 처리하는 데 사용됩니다.

더 상호작용적인 예제를 위해, 사용자가 클릭을 통해 난수 생성을 트리거하는 시나리오를 살펴보겠습니다:

```Elm
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Random

type alias Model = Int

type Msg = Generate

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Generate ->
            (model, Random.generate NewRandomNumber (Random.int 1 100))

view : Model -> Html Msg
view model =
    div []
        [ text ("Generated number: " ++ String.fromInt model)
        , button [ onClick Generate ] [ text "Generate new number" ]
        ]

type Msg = NewRandomNumber Int
```

이 Elm 애플리케이션은 사용자가 버튼을 클릭할 때마다 새로운 난수로 디스플레이를 업데이트하여 상호작용성을 도입합니다.

## 심층 탐구
Elm의 난수 생성 시스템 설계는 언어의 순수성과 예측 가능성에 대한 약속에서 비롯됩니다. 각 호출마다 다른 값을 반환하는 직접적인, 순수하지 않은 함수 대신, Elm은 난수를 `Cmd` 구조체에 캡슐화하여 부작용을 순수 함수와 분리하는 아키텍처와 일치시킵니다.

이 접근법은 애플리케이션 동작의 일관성을 보장하고 디버깅을 용이하게 하지만, 명령적 난수 생성에 익숙한 사람들에게 학습 곡선을 도입합니다. 그러나, 애플리케이션의 순수성을 유지하고 테스트의 용이성을 강조하는 이점은 종종 초기의 복잡성을 상쇄합니다.

전역 난수 생성기를 제공하는 언어와 비교할 때, Elm의 방법은 공유 상태로 인해 발생할 수 있는 미묘한 버그로부터 벗어납니다. 난수 생성과 그 효과의 명시적 처리를 요구함으로써 Elm은 개발자들이 어디에서 어떻게 난수가 그들의 애플리케이션에 영향을 미치는지에 대해 더 비판적으로 생각하게 하여 더 견고하고 예측 가능한 코드로 이어지게 합니다.

대안으로, 다른 함수형 언어들은 비슷한 기능을 제공하지만 다르게 구현할 수 있습니다. 예를 들어, Haskell도 난수 생성에서 순수성을 유지하지만, Elm이 의도적으로 간소화된 모델을 위해 피하는 모나드 사용을 통해 이를 달성합니다. 비교적으로 Elm의 접근 방식은 신규 사용자에게 더 접근하기 쉬우며, 함수형 프로그래밍 원칙의 힘을 희생하지 않으면서도 간단한 애플리케이션 아키텍처를 강조합니다.
