---
title:                "텍스트 파일 작성하기"
date:                  2024-01-19
html_title:           "Arduino: 텍스트 파일 작성하기"
simple_title:         "텍스트 파일 작성하기"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
텍스트 파일 작성은 정보를 저장하고 유지하는 방법입니다. 프로그래머들이 자료를 영구적으로 보존하거나 다른 시스템과 공유할 필요가 있을 때 사용합니다.

## How to: (방법)
Elm에서 직접적인 파일 시스템 접근을 허용하지 않습니다. 대신, JavaScript와 상호 작용하는 방법을 사용해야 합니다. `Ports`를 통해 JavaScrip와 Elm 사이에 메시지를 보내 파일을 저장합니다.

```Elm
port module Main exposing (..)

-- Port를 선언하여 Elm에서 JavaScript로 메시지를 보냅니다.
port saveFile : String -> Cmd msg

-- 파일을 저장하는 함수
saveTextToFile : String -> Cmd msg
saveTextToFile text =
    saveFile text

-- 예시 사용
main =
    saveTextToFile "안녕하세요! Elm으로 파일을 저장합니다."
```

JavaScript에서는 다음과 같이 `saveFile` port를 구성합니다.

```JavaScript
// Elm에서 받은 데이터로 파일을 저장합니다.
app.ports.saveFile.subscribe(function(text) {
  // 예를 들어, 파일 저장 로직을 여기에 구현합니다.
  // 여기서는 단순히 콘솔에 출력
  console.log("저장할 텍스트: ", text);
  // 파일 저장 로직 추가 대신
});
```

## Deep Dive (심화 탐구)
Elm은 순수 함수형 언어이기 때문에 사이드 이펙트를 일으킬 수 있는 파일 시스템의 직접적인 조작을 허용하지 않습니다. 이는 Elm의 안정성을 높이지만 파일 작성 같은 기능을 구현할 때 JavaScript와의 연동이 필요합니다. Elm 0.19에서는 `Native Modules` 사용이 제한되고, 이제 `Port`를 통해서만 JavaScript와의 상호 작용이 가능합니다.

## See Also (참고 자료)
- [Elm 공식 가이드](https://guide.elm-lang.org/)
- [Elm Ports](https://guide.elm-lang.org/interop/ports.html)
