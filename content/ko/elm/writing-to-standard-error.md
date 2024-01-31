---
title:                "표준 오류로 쓰기"
date:                  2024-01-19
html_title:           "Bash: 표준 오류로 쓰기"
simple_title:         "표준 오류로 쓰기"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?

표준 오류 출력(standard error)란, 프로그램에서 문제가 있을 때 오류 메시지를 표시하기 위한 특별한 출력 채널입니다. 이건 일반 출력과 별개로 작동하며, 문제 발생시 오류 추적과 로깅을 용이하게 하기 위해 사용합니다.

## How to:

Elm에서는 자바스크립트와의 통신을 사용하여 표준 오류를 사용할 수 있습니다. 예를 들어, `Ports`를 사용해볼 수 있습니다:

```Elm
port module Main exposing (..)

-- 포트 설정을 통해 JavaScript에 메시지를 보냅니다.
port error : String -> Cmd msg

-- 오류를 출력하는 함수
printError : String -> Cmd msg
printError message =
    error message

-- 샘플 사용
main =
    printError "이것은 오류 메시지입니다."
```

JavaScript에서는 다음과 같이 Elm으로부터 오는 메시지를 받아 처리할 수 있습니다:

```JavaScript
app.ports.error.subscribe(function(message) {
    console.error(message);
});
```

위 Elm 코드의 `printError` 함수를 호출하면 JavaScript 콘솔에 "이것은 오류 메시지입니다." 가 표준 오류로 출력됩니다.

## Deep Dive

표준 오류(standard error, stderr)는 UNIX와 같은 시스템에서 처음 도입되어, 출력(stdout)과는 별도로 에러 메시지를 관리할 수 있도록 했습니다. Elm은 웹 프론트엔드 언어로서 직접적인 stderr 쓰기를 지원하지 않고, 대신 JavaScript와의 인터오프를 통해 이러한 기능을 앱에 구현할 수 있습니다. `Ports`나 `Custom Elements`를 활용하는 것이 대체 방법입니다. 표준 오류에 직접 접근할 수 없는 것은 Elm의 안전성과 철저한 타입 검사 원칙을 반영합니다.

## See Also

- Elm 공식 문서의 Ports 가이드: [https://guide.elm-lang.org/interop/ports.html](https://guide.elm-lang.org/interop/ports.html)
- Elm에서 JavaScript로의 통신 방법에 대해 설명하는 Elm의 Interop 예제들: [https://elm-lang.org/examples](https://elm-lang.org/examples)
- 커스텀 엘리먼트를 사용하는 방법에 대한 자료: [https://guide.elm-lang.org/interop/custom_elements.html](https://guide.elm-lang.org/interop/custom_elements.html)
