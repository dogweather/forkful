---
title:                "컴퓨터 프로그래밍의 제목: 커맨드 라인 인수 읽기"
html_title:           "Elm: 컴퓨터 프로그래밍의 제목: 커맨드 라인 인수 읽기"
simple_title:         "컴퓨터 프로그래밍의 제목: 커맨드 라인 인수 읽기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

이번 글은 Elm 언어에서 커맨드 라인 인자를 읽는 방법을 쉽고 빠르게 익힐 수 있도록 안내합니다. 커맨드 라인 인자는 프로그래밍에서 꼭 필요한 요소이며, 이 글을 읽으심으로써 이를 활용하는 방법을 배우고 더 나은 프로그래밍을 할 수 있을 겁니다.

## 어떻게

커맨드 라인 인자를 읽는 방법은 간단합니다. 먼저, `Elm.Command` 모듈을 불러와 줍니다. 그리고 `Elm.Command.program` 함수를 호출하면서 읽어들이고 싶은 타입을 인수로 넘깁니다. 코드로 보면 다음과 같습니다.

```Elm
import Elm.Command

type MyArgs
    = Version String
    | Help
    | Run String String

main : MyArgs -> Cmd.Cmd msg
main args =
    Elm.Command.program defaultConfig
        |> Elm.Command.argument (String)
        |> Elm.Command.version "1.0"
        |> Elm.Command.usage
        |> Elm.Command.handler
            (parseArgs args)

parseArgs : MyArgs -> Cmd.Cmd msg
parseArgs args =
    case args of
        Version v ->
            -- Version을 출력합니다.
            Cmd.none

        Help ->
            -- 도움말을 출력합니다.
            Cmd.none

        Run input output ->
            -- input을 가지고 output을 생성합니다.
            Cmd.none
```

위 코드에서 `main` 함수는 `Elm.Command.program` 함수를 이용해서 `defaultConfig`와 함께 읽어들이고 싶은 타입을 지정해줍니다. 그 이후 `Elm.Command.argument` 함수를 통해 어떤 타입을 받을지 지정하고, `Elm.Command.version` 함수를 이용해서 버전 정보를 추가해 줍니다. 그리고 마지막으로 `Elm.Command.handler` 함수를 이용해서 인자를 처리할 코드를 작성해줍니다. 여기서는 `parseArgs` 함수를 호출하여 인자를 패턴 매칭해줍니다. 이제 커맨드 라인에서 `--version`, `--help`, 그리고 `--run` 옵션으로 프로그램을 실행시켜보세요. 각각의 옵션이 제대로 처리되는 것을 확인할 수 있을 겁니다.

## 깊이 파보기

이번에는 인자를 추가적으로 읽는 방법에 대해 알아보겠습니다. `Elm.Command.argument` 함수는 어떤 타입을 받을지를 인수로 전달하는 함수입니다. 그런데 이 함수는 단일 타입만 받을 수 있습니다. 그러면 여러 개의 타입을 받을 수는 없을까요? 그럴 때 사용하는 함수가 바로 `Elm.Command.arguments` 함수입니다. 이 함수는 뒤에 `...`를 붙여 여러 개의 인자를 한 번에 받을 수 있도록 해줍니다. 코드로 보면 다음과 같습니다.

```Elm
main : List String -> Cmd.Cmd msg
main inputs =
    -- 인자를 읽습니다.
    Cmd.none

    -- 인자를 출력합니다.
    | Cmd.map Debug.log inputs
```

이 코드에서는 `main` 함수의 인수로 `List String`을 받도록 지정해줍니다. 그리고 나서 `Cmd.none`을 호출하는데, 이는 인자를 읽는 작업이 모두 끝난 뒤에 호출됩니다. 이 코드를 컴파일해서 실행하면 인자로 넘긴 값들이 출력되는 것을 확인할 수 있을 겁니다.

## 더 알아보기

만약 커맨드 라인 인자를 읽는 방법에 대해 더 알고 싶다면 아래의 링크들을 참