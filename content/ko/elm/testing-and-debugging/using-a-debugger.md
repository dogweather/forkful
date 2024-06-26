---
date: 2024-01-26 03:49:23.496531-07:00
description: "\uBC29\uBC95: Elm\uC740 \uC804\uD1B5\uC801\uC778 \uC758\uBBF8\uC5D0\uC11C\
  \ \uC790\uBC14\uC2A4\uD06C\uB9BD\uD2B8\uAC00 \uBE0C\uB77C\uC6B0\uC800 \uAC1C\uBC1C\
  \ \uB3C4\uAD6C\uB85C \uAC00\uC9C0\uACE0 \uC788\uB294 \uAC83\uCC98\uB7FC \uB0B4\uC7A5\
  \ \uB514\uBC84\uAC70\uB97C \uAC00\uC9C0\uACE0 \uC788\uC9C0 \uC54A\uC2B5\uB2C8\uB2E4\
  . \uD558\uC9C0\uB9CC, Elm \uCEE4\uBBA4\uB2C8\uD2F0\uB294 \uC774 \uACF5\uBC31\uC744\
  \ \uCC44\uC6B0\uAE30 \uC704\uD55C \uB3C4\uAD6C\uB97C \uB9CC\uB4E4\uC5C8\uC2B5\uB2C8\
  \uB2E4. \uC5EC\uAE30 `elm-debug-transformer`\uB97C \uC0AC\uC6A9\uD558\uC5EC Elm\
  \ \uC571\uC744\u2026"
lastmod: '2024-03-13T22:44:55.117104-06:00'
model: gpt-4-0125-preview
summary: "Elm\uC740 \uC804\uD1B5\uC801\uC778 \uC758\uBBF8\uC5D0\uC11C \uC790\uBC14\
  \uC2A4\uD06C\uB9BD\uD2B8\uAC00 \uBE0C\uB77C\uC6B0\uC800 \uAC1C\uBC1C \uB3C4\uAD6C\
  \uB85C \uAC00\uC9C0\uACE0 \uC788\uB294 \uAC83\uCC98\uB7FC \uB0B4\uC7A5 \uB514\uBC84\
  \uAC70\uB97C \uAC00\uC9C0\uACE0 \uC788\uC9C0 \uC54A\uC2B5\uB2C8\uB2E4."
title: "\uB514\uBC84\uAC70 \uC0AC\uC6A9\uD558\uAE30"
weight: 35
---

## 방법:
Elm은 전통적인 의미에서 자바스크립트가 브라우저 개발 도구로 가지고 있는 것처럼 내장 디버거를 가지고 있지 않습니다. 하지만, Elm 커뮤니티는 이 공백을 채우기 위한 도구를 만들었습니다. 여기 `elm-debug-transformer`를 사용하여 Elm 앱을 디버깅하는 방법이 있습니다:

```Elm
-- elm-debug-transformer 설치 (Node 패키지)

1. npm install -g elm-debug-transformer

-- elm-debug-transformer로 앱 시작하기

2. elm-debug-transformer --port=8000 yourMainElmFile.elm 
```

`elm-debug-transformer`가 실행되면, 로깅을 위한 WebSocket 연결을 만듭니다. 애플리케이션에서 특정 지점의 프로그램 데이터 구조를 검사할 수 있는 디버그 정보를 브라우저의 콘솔에서 볼 수 있습니다.

Elm 0.19 이상에서 `Debug` 모듈의 함수들인 `Debug.log`와 `Debug.todo`는 값을 추적하고 코드의 미완성 부분을 의도적으로 표시하는 데 도움을 줄 수 있습니다. 이렇게 `Debug.log`를 사용하는 방법입니다:

```Elm
import Debug

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( Debug.log "Incrementing" { model | count = model.count + 1 }, Cmd.none )

        Decrement ->
            ( Debug.log "Decrementing" { model | count = model.count - 1 }, Cmd.none )
```

브라우저의 콘솔에서 "Incrementing" 또는 "Decrementing" 메시지와 함께 `model`의 새로운 상태를 볼 수 있습니다.

## 심층 분석
Elm의 작성자 Evan Czaplicki는 일반적인 버그를 불가능하게 하거나 쉽게 잡을 수 있는 언어를 만드는 것을 목표로 했습니다. 이 철학이 Elm의 핵심이 전통적인 디버깅 함수를 포함하지 않는 이유입니다. Elm의 정적 분석과 타입 추론은 런타임 오류를 대폭 줄이는 데 크게 기여하여, 복잡한 런타임 디버깅이 필요한 경우를 줄입니다. 이전에는 지금은 사용되지 않는 `elm-reactor`를 사용하여 시간 여행 디버깅 - 앱에서 작업을 되감고 재생하는 방법을 제공했습니다.

오늘날, `elm-debug-transformer` 같은 도구와 Elm의 `Debug` 모듈 사용은 이 공백을 메워줍니다. `Debug` 모듈은 개발 중에만 사용되어야 하며, 프로덕션 빌드 전에 제거해야 하지만, 상태 변경을 정확하게 포착하고 로깅하는 데 있어 가치 있는 도구입니다.

Elm의 아키텍처와 Elm 런타임이 상태 업데이트를 처리하는 방식 때문에, 전통적인 자바스크립트 디버깅 기법들, 예를 들어 중단점이나 단계별 실행은 Elm에서 직접 적용될 수 없습니다. Elm은 데이터 흐름이 명확하고 엄격한 타입과 변경 불가능성 보장을 따르도록 프로그램을 구조화하도록 권장하여 디버깅이 필요한 경우를 최소화합니다.

## 참고 자료
- 런타임 예외 처리에 관한 Elm 공식 가이드: https://guide.elm-lang.org/error_handling/
- `elm-debug-transformer` GitHub 저장소: https://github.com/kraklin/elm-debug-transformer
- 디버깅 전략을 논의하는 Elm 디스코스 스레드: https://discourse.elm-lang.org/c/show-and-tell/debugging
- Elm의 `Debug` 모듈 문서: https://package.elm-lang.org/packages/elm/core/latest/Debug
