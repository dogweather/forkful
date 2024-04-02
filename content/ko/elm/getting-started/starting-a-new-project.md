---
date: 2024-01-20 18:03:35.581762-07:00
description: "\uC0C8 \uD504\uB85C\uC81D\uD2B8\uB97C \uC2DC\uC791\uD55C\uB2E4\uB294\
  \ \uAC74, \uBE48 \uCE94\uBC84\uC2A4\uC5D0 \uADF8\uB9BC\uC744 \uADF8\uB9AC\uAE30\
  \ \uC2DC\uC791\uD558\uB294 \uAC83\uACFC \uAC19\uC544\uC694. \uD504\uB85C\uADF8\uB798\
  \uBA38\uB4E4\uC774 \uC0C8 \uD504\uB85C\uC81D\uD2B8\uB97C \uC2DC\uC791\uD558\uB294\
  \ \uC774\uC720\uB294 \uC544\uC774\uB514\uC5B4\uB97C \uD604\uC2E4\uB85C \uB9CC\uB4E4\
  \uAC70\uB098 \uBB38\uC81C\uB97C \uD574\uACB0\uD558\uAE30 \uC704\uD574\uC11C\uC8E0\
  ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.111509-06:00'
model: gpt-4-1106-preview
summary: "\uC0C8 \uD504\uB85C\uC81D\uD2B8\uB97C \uC2DC\uC791\uD55C\uB2E4\uB294 \uAC74\
  , \uBE48 \uCE94\uBC84\uC2A4\uC5D0 \uADF8\uB9BC\uC744 \uADF8\uB9AC\uAE30 \uC2DC\uC791\
  \uD558\uB294 \uAC83\uACFC \uAC19\uC544\uC694. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\
  \uC774 \uC0C8 \uD504\uB85C\uC81D\uD2B8\uB97C \uC2DC\uC791\uD558\uB294 \uC774\uC720\
  \uB294 \uC544\uC774\uB514\uC5B4\uB97C \uD604\uC2E4\uB85C \uB9CC\uB4E4\uAC70\uB098\
  \ \uBB38\uC81C\uB97C \uD574\uACB0\uD558\uAE30 \uC704\uD574\uC11C\uC8E0."
title: "\uC0C8 \uD504\uB85C\uC81D\uD2B8 \uC2DC\uC791\uD558\uAE30"
weight: 1
---

## What & Why? (무엇을? 왜?)
새 프로젝트를 시작한다는 건, 빈 캔버스에 그림을 그리기 시작하는 것과 같아요. 프로그래머들이 새 프로젝트를 시작하는 이유는 아이디어를 현실로 만들거나 문제를 해결하기 위해서죠.

## How to: (어떻게 하나요?)
Elm 프로젝트를 시작해봅시다. `Elm` 설치 후, 새 프로젝트 폴더를 만들고 `elm init`을 실행합니다.

```Elm
$ mkdir my-elm-project
$ cd my-elm-project
$ elm init
```

이렇게 하면 `elm.json` 파일과 `src` 폴더가 생깁니다. 간단한 `Hello World` 프로그램을 작성해 보겠습니다.

```Elm
module Main exposing (..)
import Html

main =
    Html.text "안녕하세요, Elm!"
```

위 코드를 `src/Main.elm`에 저장하고 실행합니다.

```Elm
$ elm make src/Main.elm --output=main.html
``` 

생성된 `main.html` 파일을 웹 브라우저에서 열면 "안녕하세요, Elm!"을 볼 수 있습니다.

## Deep Dive (자세히 알아봅시다)
Elm은 순수 함수형 프로그래밍 언어로서, 웹 프론트엔드 개발에 특화되어 있습니다. 2012년 Evan Czaplicki에 의해 시작되었죠. Elm의 가장 큰 장점 중 하나는 런타임 예외가 없다는 것입니다. 

자바스크립트나 타입스크립트와 같은 다른 언어들 대신 Elm을 사용하는 이유 중 하나는 Elm의 아키텍처와 타입 시스템 때문입니다. 이 시스템은 애플리케이션을 더 안전하고 예측 가능하게 만들어줍니다.

Elm 프로젝트를 시작할 때, `elm.json` 파일은 중요한 역할을 합니다. 이 파일은 프로젝트의 의존성과 버전 정보를 관리하죠. 프로젝트를 구성하는 데 필요한 모든 세부 정보가 여기에 들어 있습니다.

## See Also (더 알아보기)
- [Elm 공식 홈페이지](https://elm-lang.org/)
- [Elm 패키지 매니저](https://package.elm-lang.org/)
- [Beginner's Guide to Elm](http://elmprogramming.com/)
- [Elm Tutorial by Pragmatic Studio](https://pragmaticstudio.com/elm)
