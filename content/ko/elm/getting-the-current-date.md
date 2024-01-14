---
title:                "Elm: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜 Date를 얻는가?

현재 날짜를 얻는 것은 Elm 프로그래머들에게 중요한 작업 중 하나입니다. Date는 사용자에게 현재 시간을 보여주는 데 필요한 정보를 제공합니다. 또한 날짜를 계산하거나 표시하는 데 사용될 수도 있습니다.

## 어떻게 Date를 얻을 수 있나요?

Date는 Elm 시간 모듈에서 제공하는 함수들을 사용하여 얻을 수 있습니다. 첫 번째 단계는 Time 모듈을 가져오는 것입니다. 그런 다음 현재 날짜를 얻는 함수인 `now`를 사용하여 Date 데이터를 얻을 수 있습니다. 아래의 예시 코드를 참고해 주세요.

```Elm
import Time

currentTime : Cmd Msg
currentTime =
    Time.now
        |> Task.perform NewDateAndTime

type Msg
    = NewDateAndTime (Result String Time.Report)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NewDateAndTime (Ok date) ->
            ( { model | currentDate = Just date }, Cmd.none )

        NewDateAndTime (Err _) ->
            ( model, Cmd.none )
```

위 코드는 `update` 함수에서 `currentTime`를 호출하고, 해당 함수를 수행하는 `Msg`를 업데이트합니다. 이제 `Model`에 `currentDate`가 추가되어 컴포넌트에서 이를 사용할 수 있게 됩니다. 아래는 샘플 출력입니다.

```Elm
{ currentDate = Just (2019, 8, 27, 13, 45) }
```

이 출력에서 날짜와 시간을 구성하는 값들이 튜플 형태로 표시됩니다. 이 값들은 컴포넌트에서 그대로 사용할 수 있으며, 날짜와 시간을 원하는 형태로 표시하거나 계산할 수 있습니다.

## 딥 다이브

Date를 얻고 사용하는 데는 다양한 함수들이 있으며, 이를 조합하여 원하는 결과를 얻을 수 있습니다. Date의 연산은 `Time` 모듈의 `add` 함수를 사용하면 됩니다. 이 함수를 사용하면 날짜를 더하거나 빼는 등 다양한 연산을 수행할 수 있습니다.

## 더 알아보기

- [Elm Time 모듈 공식 문서](https://package.elm-lang.org/packages/elm/time/latest)
- [Elm 날짜 계산 예시 블로그 포스트](https://medium.com/@ashishchoyal999/elm-date-manipulation-e1c8e3b4c293)
- [Elm 시간과 날짜 관련 라이브러리 모음](https://www.elm-plugins.com/)
- [Elm 커뮤니티 Slack 채널](https://elmlang.herokuapp.com/)