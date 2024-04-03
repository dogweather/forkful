---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:02.752612-07:00
description: "\uC5B4\uB5BB\uAC8C: Elm\uC740 \uBE0C\uB77C\uC6B0\uC800\uC5D0\uC11C \uC2E4\
  \uD589\uB418\uBA70 \uBD80\uC791\uC6A9\uC774 \uC5C6\uB294 \uC21C\uC218 \uD504\uB85C\
  \uADF8\uB798\uBC0D \uC5B8\uC5B4\uB85C \uC124\uACC4\uB418\uC5C8\uAE30 \uB54C\uBB38\
  \uC5D0 \uD30C\uC77C \uC2DC\uC2A4\uD15C\uC5D0 \uC9C1\uC811 \uC811\uADFC\uD560 \uC218\
  \ \uC5C6\uC2B5\uB2C8\uB2E4. \uB530\uB77C\uC11C \uD30C\uC77C\uC5D0 \uC4F0\uB294 \uAC83\
  \uC740 \uC77C\uBC18\uC801\uC73C\uB85C JavaScript\uB85C \uB370\uC774\uD130\uB97C\
  \ \uC804\uC1A1\uD558\uB294 \uD3EC\uD2B8\uB97C \uD1B5\uD574 \uC774\uB8E8\uC5B4\uC9D1\
  \uB2C8\uB2E4. \uB2E4\uC74C\uC740 \uC774\uB97C \uC124\uC815\uD558\uB294 \uBC29\uBC95\
  \uC785\uB2C8\uB2E4: 1.\u2026"
lastmod: '2024-03-13T22:44:55.137194-06:00'
model: gpt-4-0125-preview
summary: "Elm\uC740 \uBE0C\uB77C\uC6B0\uC800\uC5D0\uC11C \uC2E4\uD589\uB418\uBA70\
  \ \uBD80\uC791\uC6A9\uC774 \uC5C6\uB294 \uC21C\uC218 \uD504\uB85C\uADF8\uB798\uBC0D\
  \ \uC5B8\uC5B4\uB85C \uC124\uACC4\uB418\uC5C8\uAE30 \uB54C\uBB38\uC5D0 \uD30C\uC77C\
  \ \uC2DC\uC2A4\uD15C\uC5D0 \uC9C1\uC811 \uC811\uADFC\uD560 \uC218 \uC5C6\uC2B5\uB2C8\
  \uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC4F0\uAE30"
weight: 24
---

## 어떻게:
Elm은 브라우저에서 실행되며 부작용이 없는 순수 프로그래밍 언어로 설계되었기 때문에 파일 시스템에 직접 접근할 수 없습니다. 따라서 파일에 쓰는 것은 일반적으로 JavaScript로 데이터를 전송하는 포트를 통해 이루어집니다. 다음은 이를 설정하는 방법입니다:

1. **JavaScript로 텍스트를 보내는 포트 모듈 정의:**

```elm
port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

-- JavaScript로 텍스트 데이터를 보내는 포트 정의
port saveText : String -> Cmd msg

-- 메인 뷰
view : Html msg
view =
    div []
        [ button [ onClick (saveText "Hello, Elm writes to a file!") ] [ text "파일로 저장" ]
        ]

-- 구독 설정 (이 예제에서는 사용되지 않지만 포트 모듈에 필요)
subscriptions : model -> Sub msg
subscriptions _ =
    Sub.none

-- 애플리케이션 설정
main : Program () model msg
main =
    Browser.element
        { init = \_ -> ((), Cmd.none)
        , view = \_ -> view
        , update = \_ _ -> ((), Cmd.none)
        , subscriptions = subscriptions
        }
```

2. **해당 JavaScript 코드 구현:**

HTML 파일이나 JavaScript 모듈에서 Elm 애플리케이션의 텍스트를 저장하기 위한 포트를 처리합니다. 클라이언트 측에서 파일을 저장하기 위해 `FileSaver.js` 라이브러리를 사용하거나 데이터를 서버로 전송해 처리할 수 있습니다.

```javascript
// Elm.Main.init()이 이미 호출되었고 앱이 실행 중이라고 가정
app.ports.saveText.subscribe(function(text) {
    // 클라이언트 측에서 파일을 저장하기 위해 FileSaver.js 사용
    var blob = new Blob([text], {type: "text/plain;charset=utf-8"});
    saveAs(blob, "example.txt");
});
```

샘플 출력은 파일 생성이 결과이므로 직접 적용할 수 없습니다. 그러나 Elm 애플리케이션에서 버튼을 클릭한 후, "Hello, Elm writes to a file!"이라는 문자열을 포함하는 "example.txt"라는 이름의 파일이 컴퓨터에 다운로드됩니다.

이 접근 방식에서 Elm과 JavaScript 사이의 커뮤니케이션은 필수적입니다. Elm은 가능한 한 애플리케이션의 로직을 포함하려고하지만, 포트를 통한 JavaScript와의 상호 운용은 Elm이 직접 지원하지 않는 작업들, 예를 들어 파일 쓰기 등을 수행할 수 있게 합니다. Elm의 순수성과 안전성은 이 패턴을 통해 향상되며, 복잡한 외부 세계와 상호 작용할 때조차도 Elm 애플리케이션이 유지보수하고 이해하기 쉽게 유지되도록 보장합니다.
