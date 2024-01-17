---
title:                "컴퓨터 프로그래밍을 위한 명령 줄 인수 읽기"
html_title:           "Elm: 컴퓨터 프로그래밍을 위한 명령 줄 인수 읽기"
simple_title:         "컴퓨터 프로그래밍을 위한 명령 줄 인수 읽기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 무엇과 왜?

커맨드 라인 인수를 읽는 것은 프로그래머가 사용자로부터 정보를 입력받는 방법입니다. 이는 사용자가 프로그램을 실행할 때 추가적인 정보를 제공하거나 설정을 변경할 때 유용합니다.

## 하는 법:

```Elm
import Platform
import Html exposing (text)

main : Program flags
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }

init : flags -> ( Model, Cmd Msg )
init flags =
    -- flags 값으로부터 정보를 읽어올 수 있습니다.
    let
        argument1 =
            flags.argument1

        argument2 =
            flags.argument2

        argument3 =
            flags.argument3
    in
    -- 이후 해당 값들을 모델로 설정하거나, 업데이트 함수를 통해 사용할 수 있습니다.
    ( Model argument1 argument2 argument3, Cmd.none )


type Msg
    = DoSomething

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DoSomething ->
          -- 모델의 값을 바탕으로 원하는 작업을 수행할 수 있습니다.
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

```

Output: 해당 커맨드 라인 인수를 사용하여 프로그램의 모델을 초기화하고, 업데이트 함수를 통해 모델의 값들을 수정하고자 할 때 사용할 수 있습니다.

## 심층 분석:

커맨드 라인 인수를 읽는 기능은 컴퓨터 시스템에서 입력 값을 받는 가장 기본적인 방법 중 하나입니다. 이전에는 입력 값을 읽는데 사용되는 프로그래밍 언어에 따라 정해진 방식이 있었지만, 초창기 개인용 컴퓨터에서는 파일 입출력 등의 다른 방법이 없어 이 방식이 최선의 선택이었습니다. 하지만 현재 Elm을 포함한 많은 프로그래밍 언어들은 다른 방식의 입력 값 읽기 기능을 제공하고 있습니다.

이와 비슷한 기능을 구현하기 위해서는 다른 함수나 라이브러리를 사용할 수 있지만, 커맨드 라인 인수를 읽는 기능은 직접 구현하는 것이 더 간단하고 효율적입니다. 해당 기능은 Elm에서 제공하는 Platform 모듈을 사용하여 구현할 수 있으며, 코드의 범용성을 높히기 위해 플래그 값으로 값을 읽어오는 방식을 사용합니다.

## 관련 자료:

- [Elm의 Platform 모듈 문서](https://package.elm-lang.org/packages/elm/core/latest/Platform)