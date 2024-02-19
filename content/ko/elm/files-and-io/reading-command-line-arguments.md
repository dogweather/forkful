---
aliases:
- /ko/elm/reading-command-line-arguments/
date: 2024-01-20 17:56:32.818204-07:00
description: "\uBA85\uB839\uC904 \uC778\uC218\uB294 \uD504\uB85C\uADF8\uB7A8\uC5D0\
  \ \uC785\uB825\uC73C\uB85C \uB118\uACA8\uC8FC\uB294 \uAC12\uB4E4\uC785\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB294 \uC0AC\uC6A9\uC790\uC758 \uC694\uAD6C\uB97C\
  \ \uB3D9\uC801\uC73C\uB85C \uBC1B\uC544\uB4E4\uC5EC \uC720\uC5F0\uD55C \uB3C4\uAD6C\
  \uB97C \uB9CC\uB4E4\uAE30 \uC704\uD574 \uC774\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4\
  ."
isCJKLanguage: true
lastmod: 2024-02-18 23:09:06.109077
model: gpt-4-1106-preview
summary: "\uBA85\uB839\uC904 \uC778\uC218\uB294 \uD504\uB85C\uADF8\uB7A8\uC5D0 \uC785\
  \uB825\uC73C\uB85C \uB118\uACA8\uC8FC\uB294 \uAC12\uB4E4\uC785\uB2C8\uB2E4. \uD504\
  \uB85C\uADF8\uB798\uBA38\uB294 \uC0AC\uC6A9\uC790\uC758 \uC694\uAD6C\uB97C \uB3D9\
  \uC801\uC73C\uB85C \uBC1B\uC544\uB4E4\uC5EC \uC720\uC5F0\uD55C \uB3C4\uAD6C\uB97C\
  \ \uB9CC\uB4E4\uAE30 \uC704\uD574 \uC774\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uBA85\uB839\uC904 \uC778\uC218 \uC77D\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜?)
명령줄 인수는 프로그램에 입력으로 넘겨주는 값들입니다. 프로그래머는 사용자의 요구를 동적으로 받아들여 유연한 도구를 만들기 위해 이를 사용합니다.

## How to:
Elm은 주로 웹 프론트엔드 개발에 사용되므로, 명령줄 인수를 직접 읽는 기능을 내장하고 있지 않습니다. 그러나 Elm을 Node.js와 함께 사용하는 방법으로 명령줄 인수를 다룰 수 있습니다. 아래는 `elm/interop` 패키지를 사용한 예시입니다.

```Elm
port module Main exposing (..)

import Json.Decode as Decode
import Platform

port cmdlineArgs : (Decode.Value -> msg) -> Sub msg

type Msg
    = ReceiveCmdLineArgs (List String)

subscriptions : Model -> Sub Msg
subscriptions model =
    cmdlineArgs (Decode.list Decode.string |> Decode.map ReceiveCmdLineArgs)

main =
    Platform.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
        
-- 해당 코드는 Node.js 환경에서 실행되어야 합니다.
```

노드 환경에서 `node index.js arg1 arg2 arg3` 같이 실행하면, Elm 프로그램은 리스트 `["arg1", "arg2", "arg3"]`를 `ReceiveCmdLineArgs` 메세지로 받게 됩니다.

## Deep Dive (심층 탐구)
Elm이 웹 브라우저를 위해 설계되었기 때문에, 직접적인 명령줄 인수 처리 기능이 내장되어 있지 않습니다. 이는 Elm의 안전성과 예측 가능성에 중점을 둔 설계 철학과 관련이 있습니다. 대안으로, Elm에서 포트를 사용해 JavaScript와의 상호 운용성을 활용할 수 있습니다. 이를 통해 Node.js에서 Elm 애플리케이션이 명령줄 인수를 읽을 수 있습니다. 포트를 통한 데이터 교환은 타입 안전성을 보장하면서 두 언어 간 연동하게 해줍니다. 

## See Also (참조)
- Elm 공식 문서: [https://guide.elm-lang.org/interop/](https://guide.elm-lang.org/interop/)
- Elm Ports 예제: [https://guide.elm-lang.org/interop/ports.html](https://guide.elm-lang.org/interop/ports.html)
