---
date: 2024-01-20 15:14:03.170530-07:00
description: "How to: (\uBC29\uBC95) Elm\uC5D0\uC11C \uD604\uC7AC \uB0A0\uC9DC\uB97C\
  \ \uC5BB\uB294 \uBC29\uBC95\uC740 `Time` \uBAA8\uB4C8\uC744 \uC0AC\uC6A9\uD558\uB294\
  \ \uAC83\uC785\uB2C8\uB2E4. \uC9C1\uC811 \uCF54\uB4DC\uB97C \uBCF4\uBA70 \uB530\uB77C\
  \ \uD574\uBCF4\uC138\uC694."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.877038-06:00'
model: unknown
summary: "(\uBC29\uBC95) Elm\uC5D0\uC11C \uD604\uC7AC \uB0A0\uC9DC\uB97C \uC5BB\uB294\
  \ \uBC29\uBC95\uC740 `Time` \uBAA8\uB4C8\uC744 \uC0AC\uC6A9\uD558\uB294 \uAC83\uC785\
  \uB2C8\uB2E4."
title: "\uD604\uC7AC \uB0A0\uC9DC \uAC00\uC838\uC624\uAE30"
weight: 29
---

## How to: (방법)
Elm에서 현재 날짜를 얻는 방법은 `Time` 모듈을 사용하는 것입니다. 직접 코드를 보며 따라 해보세요.

```Elm
import Browser
import Html
import Task
import Time

type Msg = GetCurrentTime Time.Posix

update : Msg -> Model -> (Model, Cmd Msg)
update (GetCurrentTime posix) model =
    ( posix, Cmd.none )

view : Model -> Html.Html Msg
view model =
    Html.text (Time.toIsoString model)

main =
    Browser.element
        { init = \_ -> (Time.millisToPosix 0, Task.perform GetCurrentTime Time.now)
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
```

위 코드를 실행하면 화면에 ISO 8601 형식의 현재 날짜와 시간이 출력됩니다 (예: "2023-04-24T12:34:56.789Z").

## Deep Dive (심층 탐구)
과거에 Elm에서 현재 날짜를 얻는 과정은 오늘날보다 복잡했습니다. 0.19 버전에서 `Time.now`는 `Task`를 반환하기 때문에 비동기 작업으로 처리되어야 합니다. 대안으로, 당신은 서버 시간을 API 요청을 통해 얻을 수도 있습니다. 하지만 `Time` 모듈을 사용하면 클라이언트 측에서 직접 처리할 수 있다는 장점이 있습니다.

Elm에서 날짜와 시간을 다루는 것은 순수함수적인 특성 때문에 몇 가지 제한이 있습니다. 예를 들어, 내장된 날짜 함수가 상태를 변경하거나 예측할 수 없는 결과를 내놓는 것을 방지합니다.

## See Also (더 보기)
- Elm의 공식 `Time` 모듈 문서: [Time Documentation](https://package.elm-lang.org/packages/elm/time/latest/)
- ISO 8601 날짜 표준에 대해 더 알아보기: [ISO 8601 Wikipedia](https://en.wikipedia.org/wiki/ISO_8601)
