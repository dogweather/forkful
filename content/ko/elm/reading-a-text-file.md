---
title:                "텍스트 파일 읽기"
html_title:           "Bash: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 무엇과 왜?

텍스트 파일 읽기는 컴퓨터에 저장된 텍스트 파일의 콘텐츠를 가져와 프로그램에서 사용할 수 있도록 하는 과정입니다. 이는 정보를 가져오고 처리하며 데이터 분석에 필요한 중요한 단계입니다.

## 방법:

Elm은 웹 브라우저 환경에서 실행되기 때문에 파일 시스템에 직접 접근할 수 없습니다. 대신, JavaScript와의 상호작용을 통해 텍스트 파일을 로드할 수 있습니다. 

Elm의 `port`를 이용하여 JavaScript 함수와 연결하는 예제코드는 다음과 같습니다.

```Elm
port module Main exposing (..)

port sendText : (String -> msg) -> Sub msg

type Msg =
    TextFromFile String

subscriptions : Model -> Sub Msg
subscriptions _ = 
    sendText TextFromFile
```

이 Elm 코드를 사용하면 JavaScript 컨텍스트에서 텍스트 파일의 내용을 가져와 Elm으로 전송할 수 있습니다.

## 깊이 들여다 보기:

텍스트 파일 읽기는 컴퓨터 프로그래밍의 원조 기술이며, 많은 언어가 이를 지원합니다. Elm는 Purely Functional, Statically Typed Language로써, 직접적으로 파일 시스템을 조작하는 기능을 제공하지 않습니다. 이것은 Elm 종단의 순수성과 예측 가능성 유지에 도움이 됩니다. 하지만, 포트를 통해 JavaScript와 소통함으로써 이 제한을 해결할 수 있습니다.

## 추가로 확인해볼 것들:

- Elm 포트에 대한 더 많은 정보를 얻기 위해 Elm Guide의 [Interoperability](https://guide.elm-lang.org/interop/) 섹션을 참조하십시오. 
- 브라우저에서 JavaScript를 사용하여 [File API](https://developer.mozilla.org/ko/docs/Web/API/File/Using_files_from_web_applications) 을 사용하여 텍스트 파일을 읽는 방법에 대한 자세한 정보를 찾을 수 있습니다.