---
title:                "Elm: 컴퓨터 프로그래밍에서 명령행 인수 읽기"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

커맨드 라인 인수를 읽는 것에 대해 더 알고 싶으신가요? 우리는 여러분에게 Elm 프로그래밍에서 커맨드 라인 인수를 잘 활용할 수 있는 방법에 대해 알려드릴 것입니다!

## 어떻게

우선, 커맨드 라인 인수를 읽는 가장 간단한 방법은 `Dev` 패키지의 `CommandLine` 모듈을 이용하는 것입니다. 아래의 코드 블록은 문자열 타입의 커맨드 라인 인수를 읽어오는 예시 코드입니다.

```Elm
import CommandLine exposing (..)

main =
  CommandLine.argument
      (map .head (CommandLine.arguments))
      |> Maybe.withDefault "No argument provided"
```
메인 함수에서 `CommandLine.argument` 함수를 호출하면, 인수를 리스트 형태로 반환합니다. 위의 코드에서는 `|> Maybe.withDefault` 함수를 이용해 리턴값이 `Nothing`일 경우에 대비하여 미리 지정해준 기본값을 사용하게 되어있습니다.

이제 커맨드 라인에서 인수를 받아올 수 있는 준비가 되었습니다. 예를 들어, `elm make src/Main.elm --output=output.html`이라는 커맨드로 실행한다면, 위의 코드블록에서는 `output.html`이라는 값이 반환될 것입니다.

## 깊게 파헤치기

`CommandLine` 모듈은 문자열 타입 외에도 `Int`, `Float`, `Bool` 등 다양한 타입의 인수를 읽어올 수 있습니다. 또한, 여러 개의 인수를 읽어올 수 있는 `multi` 함수도 제공되고 있습니다. 이 외에도 더 많은 기능을 알고 싶으시다면 [공식 문서](https://package.elm-lang.org/packages/elm/core/latest/Platform-CommandLine)를 참고해주세요.

## 함께 보기

- [Elm 커맨드 라인 사용 방법](https://elm-lang.org/news/Interactive-Command-Line)

- [Elm 커맨드 라인 파싱 라이브러리](https://package.elm-lang.org/packages/duc/elm-parsing/latest/)

- [Elm 커맨드 라인 응용 예제](https://github.com/ShadowManu/elm-command-line-arguments-example/)