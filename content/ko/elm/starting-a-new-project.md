---
title:                "Elm: 새로운 프로젝트를 시작하다."
programming_language: "Elm"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 왜

새로운 프로젝트를 시작하는 이유는 다양합니다. 어떤 사람들은 새로운 기술을 배우기 위해 도전적인 프로젝트에 참여하고, 어떤 사람들은 스스로의 아이디어를 구현하기 위해 프로젝트를 시작합니다. 또 어떤 사람들은 단순히 새로운 기술을 시험해보는 것이 즐겁기 때문에 프로젝트를 시작합니다. 이번 글에서는 Elm 프로그래밍 언어를 사용해 새로운 프로젝트를 시작하는 방법을 살펴보겠습니다.

## 어떻게 시작할까요

Elm 프로그래밍 언어를 사용해 새로운 프로젝트를 시작하는 것은 매우 간단합니다. 먼저 [Elm 설치](https://guide.elm-lang.org/install.html) 가이드를 따라 Elm 언어를 설치해야 합니다. 그 다음으로는 [Elm 아키텍쳐](https://guide.elm-lang.org/architecture/index.html)를 공부하고 앱의 기본 구조를 이해해야 합니다. 마지막으로는 코드를 작성하는 일만 남았습니다.

### Elm 패키지 관리자

Elm 언어는 패키지 관리자를 제공하여 쉽게 외부 라이브러리를 추가할 수 있도록 합니다. 패키지 관리자를 사용해 외부 라이브러리를 추가하고, 필요한 모듈을 가져와 사용할 수 있습니다. 예를 들어, [elm/http 패키지](https://package.elm-lang.org/packages/elm/http/latest)를 사용해 API 호출을 처리할 수 있습니다.

### 기본 앱 구조

모든 Elm 앱은 다음과 같은 기본 구조를 갖습니다.

```Elm
module Main exposing (..)

import Html exposing (..)


-- Model
type alias Model =
    {}


-- Msg
type Msg
    = NoOp


-- Init
init : Model
init =
    {}


-- Update
update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model


-- View
view : Model -> Html Msg
view model =
    text "Hello world!"


main : Program () Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        }
```

기본적으로 모델(`Model`), 메시지(`Msg`), 초기화(`init`), 업데이트(`update`), 그리고 뷰(`view`) 함수를 포함합니다. 이 기본 구조를 기반으로 앱의 모든 부분을 작성할 수 있습니다.

## 깊이 들어가기

Elm 언어는 함수형 프로그래밍 언어로, 객체지향 프로그래밍 언어와 다른 점이 많습니다. Elm 언어를 사용해 프로젝트를 시작하려면 함수형 프로그래밍에 대한 이해가 필요합니다. 또한 Elm 아키텍처를 이해하고 모델, 메시지, 업데이트, 그리고 뷰 함수를 올바르게 구현하는 것이 중요합니다.

또한, Elm 언어는 정적 타입을 갖기 때문에 컴파일 시 타입 에러를 잡을 수 있습니다. 따라서 프로젝트를 시작하기 전에 기본적인 타입 설계를 고려하는 것이 좋습니다.

## 더 알아보기

- [Elm 공식 가이드](https://guide.elm-lang.org) - Elm 언어로 프로그래밍을