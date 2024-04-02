---
date: 2024-01-26 01:03:49.083948-07:00
description: "\uB85C\uAE45\uC740 \uAE30\uBCF8\uC801\uC73C\uB85C \uC18C\uD504\uD2B8\
  \uC6E8\uC5B4\uAC00 \uC2E4\uD589\uB418\uB294 \uB3D9\uC548 \uBC1C\uC0DD\uD558\uB294\
  \ \uC774\uBCA4\uD2B8\uC640 \uB370\uC774\uD130 \uCD9C\uB825\uC744 \uAE30\uB85D\uD558\
  \uB294 \uACFC\uC815\uC785\uB2C8\uB2E4. \uC18C\uD504\uD2B8\uC6E8\uC5B4\uC758 \uC77C\
  \uAE30\uB77C\uACE0 \uC0DD\uAC01\uD558\uBA74 \uB429\uB2C8\uB2E4. \uD504\uB85C\uADF8\
  \uB798\uBA38\uB4E4\uC740 \uB85C\uAE45\uC744 \uC0AC\uC6A9\uD558\uC5EC \uB0B4\uBD80\
  \uC5D0\uC11C \uBB34\uC2A8 \uC77C\uC774 \uC77C\uC5B4\uB098\uACE0 \uC788\uB294\uC9C0\
  \ \uCD94\uC801\uD569\uB2C8\uB2E4 - \uBC84\uADF8 \uC218\uC815, \uC2DC\uC2A4\uD15C\
  \ \uD589\uB3D9\uC758 \uC2E4\uC2DC\uAC04 \uBAA8\uB2C8\uD130\uB9C1, \uC131\uB2A5 \uCD5C\
  \uC801\uD654\uB098\u2026"
lastmod: '2024-03-13T22:44:55.120311-06:00'
model: gpt-4-1106-preview
summary: "\uB85C\uAE45\uC740 \uAE30\uBCF8\uC801\uC73C\uB85C \uC18C\uD504\uD2B8\uC6E8\
  \uC5B4\uAC00 \uC2E4\uD589\uB418\uB294 \uB3D9\uC548 \uBC1C\uC0DD\uD558\uB294 \uC774\
  \uBCA4\uD2B8\uC640 \uB370\uC774\uD130 \uCD9C\uB825\uC744 \uAE30\uB85D\uD558\uB294\
  \ \uACFC\uC815\uC785\uB2C8\uB2E4. \uC18C\uD504\uD2B8\uC6E8\uC5B4\uC758 \uC77C\uAE30\
  \uB77C\uACE0 \uC0DD\uAC01\uD558\uBA74 \uB429\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\
  \uBA38\uB4E4\uC740 \uB85C\uAE45\uC744 \uC0AC\uC6A9\uD558\uC5EC \uB0B4\uBD80\uC5D0\
  \uC11C \uBB34\uC2A8 \uC77C\uC774 \uC77C\uC5B4\uB098\uACE0 \uC788\uB294\uC9C0 \uCD94\
  \uC801\uD569\uB2C8\uB2E4 - \uBC84\uADF8 \uC218\uC815, \uC2DC\uC2A4\uD15C \uD589\uB3D9\
  \uC758 \uC2E4\uC2DC\uAC04 \uBAA8\uB2C8\uD130\uB9C1, \uC131\uB2A5 \uCD5C\uC801\uD654\
  \uB098\u2026"
title: "\uB85C\uAE45"
weight: 17
---

## 무엇이며 왜 사용하는가?
로깅은 기본적으로 소프트웨어가 실행되는 동안 발생하는 이벤트와 데이터 출력을 기록하는 과정입니다. 소프트웨어의 일기라고 생각하면 됩니다. 프로그래머들은 로깅을 사용하여 내부에서 무슨 일이 일어나고 있는지 추적합니다 - 버그 수정, 시스템 행동의 실시간 모니터링, 성능 최적화나 감사를 위한 과거 활동 분석에 있어 가치가 높습니다.

## 사용 방법:
Elm의 아키텍처는 로깅과 같은 부수 효과(side effects)를 기본적으로 지원하지 않으므로, 이를 커맨드를 통해 다루게 됩니다. 커맨드는 어플리케이션 아키텍처의 일부입니다. 교육 목적으로, JavaScript로 메시지를 보내는 통신구(ports)를 통해 로깅을 시뮬레이션하는 방법을 살펴봅시다.

먼저, 통신구 모듈을 정의합니다:

```Elm
port module Logger exposing (..)

-- JavaScript로 로그를 보내는 통신구 정의하기
port log : String -> Cmd msg
```

`Main.elm`에서 `log` 통신구를 사용하여 로그 메시지를 보냅니다:

```Elm
import Logger exposing (log)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        AnEvent ->
            -- 모델 여기서 업데이트
            ( updatedModel, log "AnEvent occurred." )

        AnotherEvent ->
            -- 여기서 다른 모델 업데이트
            ( anotherUpdatedModel, log "AnotherEvent occurred." )
```

JavaScript 측에서는 `log` 통신구에 대한 구독을 통해 들어오는 로그 메시지를 처리합니다:

```JavaScript
var app = Elm.Main.init({ /* ... */ });

app.ports.log.subscribe(function(message) {
    console.log(message);
});
```

그러면 JavaScript 콘솔에서 다음과 같은 출력이 나타날 것입니다:

```
AnEvent occurred.
AnotherEvent occurred.
```

## 심화 학습
전통적으로 Python이나 Java와 같은 언어에서 로깅은 로그 메시지를 debug, info, warning, error, critical과 같은 여러 수준에서 보낼 수 있는 단순한 API를 제공하는 로깅 라이브러리를 사용하여 수행됩니다.

순수성과 불변성에 초점을 맞춘 Elm은 이런 직접적인 로깅을 지원하지 않으며, 모든 종류의 입출력이나 부수 효과는 Elm 아키텍처를 통해 명확하게 관리됩니다.

풀 기능을 갖춘 로깅이 Elm에서 필요할 때, 일반적으로 외부 JavaScript 도구에 의존합니다. 위에서 보여진 것처럼, 통신구는 이러한 도구로의 다리입니다. `Debug` 모듈은 또 다른 옵션입니다만, 개발용으로만 쓰이고 생산 로깅을 위한 것은 아닙니다.

통신구 외에도, 프로그래머들은 Elm 컴파일러 메시지와 실행 시간 디버깅 기능인 `Debug.log`를 종종 사용합니다. 이는 코드에 삽입하여 값을 추적하고, 표현식을 래핑하여 콘솔에 출력 결과를 로그로 남깁니다:

```Elm
view model =
    Debug.log "Model Debug" model
    -- 여기서 뷰 코드를 계속함
```

하지만 이 또한 생산용은 아닙니다. elm-logger와 같은 도구는 통신구를 위한 일부 추상화를 제공하지만, 이들 또한 개발보다는 생산에 적합하진 않습니다.

## 참고 자료
- Elm 통신구: https://guide.elm-lang.org/interop/ports.html
- Elm `Debug`: https://package.elm-lang.org/packages/elm/core/latest/Debug
- Elm과 로깅에 대한 토론: https://discourse.elm-lang.org/t/elm-and-logging/546
- JavaScript Console API: https://developer.mozilla.org/en-US/docs/Web/API/Console
- elm-logger 패키지: https://package.elm-lang.org/packages/arkgil/elm-logger/latest/
