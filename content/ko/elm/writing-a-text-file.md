---
title:                "텍스트 파일 쓰기"
aliases:
- ko/elm/writing-a-text-file.md
date:                  2024-02-03T19:28:02.752612-07:00
model:                 gpt-4-0125-preview
simple_title:         "텍스트 파일 쓰기"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇과 왜?

Elm에서 텍스트 파일을 작성한다는 것은 Elm 애플리케이션에서 텍스트 데이터를 파일에 생성하고 저장하는 것을 말합니다. 프로그래머들은 종종 다른 애플리케이션에서 사용하거나 기록 보관 목적으로 보고서, 로그, 또는 구조화된 텍스트 형식(e.g., JSON, CSV)의 데이터를 내보낼 필요가 있습니다. 그러나 Elm의 아키텍처가 순수성과 안전성에 중점을 둠에 따라, 파일 쓰기와 같은 많은 다른 부작용은 주변 JavaScript 환경에 대한 명령을 통해 처리됩니다.

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
