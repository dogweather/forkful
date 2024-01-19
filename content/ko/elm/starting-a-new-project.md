---
title:                "새 프로젝트 시작하기"
html_title:           "Arduino: 새 프로젝트 시작하기"
simple_title:         "새 프로젝트 시작하기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

프로젝트를 시작하는 것은 새로운 아이디어를 실현하기 위한 첫 단계입니다. 프로그래머들은 이를 통해 새로운 기능을 개발하거나 기존의 문제점을 해결하기 위해 프로젝트를 시작합니다.

## 어떻게:

Elm로 프로젝트를 시작하는 방법에 대한 간단한 가이드입니다.

```Elm
-- 먼저 Elm 패키지를 설치합니다.
npm install -g elm

-- 새로운 Elm 프로젝트를 시작합니다.
elm init

-- 위의 명령은 아래의 구조를 가진 프로젝트를 생성합니다: 
MyElmProject
|-- elm.json
`-- src
    `-- Main.elm
```
이제 첫번째 `Hello World` 프로그램을 작성해봅시다.

```Elm
module Main exposing (..)

import Browser
import Html exposing (Html, text)

main =
    Browser.sandbox { init = init, update = update, view = view }
   
init = 0

type Msg = NoOp

update msg model = model

view model =
    text "Hello, World! This is my first Elm app!"
```

이 코드를 `Main.elm` 파일에 입력한 후, elm-reactor를 통해 결과를 볼 수 있습니다.

```Elm
elm reactor
```
이제 웹 브라우저에서 `localhost:8000`으로 가면 `Hello, World! This is my first Elm app!` 가 보입니다.

## 깊이 들어가기:

프로젝트를 처음 시작하는 것은 프로그램의 근본적인 기초를 담당하며, 이에 따라 전체 아키텍처 및 최종 코드의 질이 결정됩니다. Elm은 2012년에 Evan Czaplicki에 의해 만들어진 함수형 프로그래밍 언어로, 프론트엔드 개발에 특화되어 있습니다.

대안으로 가장 많이 사용되는 것은 JavaScript 및 그 프레임워크들이지만, Elm은 에러 핸들링, 함수형 프로그래밍, 그리고 성능 최적화 등에서 장점을 보입니다.

## 참고 자료:

1. [Elm 공식 문서](https://elm-lang.org/docs)
2. [Elm- Architecture Tutorial](https://elmbridge.github.io/curriculum/The%20Elm%20Architecture.html)
3. [Elm Github Repository](https://www.github.com/elm/compiler)
4. [Evan Czaplicki's TEDx talk on Elm](https://www.youtube.com/watch?v=Ie-gqwSHQrM)