---
title:                "Elm: 명령 줄 인수 읽기"
simple_title:         "명령 줄 인수 읽기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

 Elm 언어를 공부하고 있다면, 여러분이 왜 프로그래밍 언어의 커맨드라인 인자를 읽을 필요가 있는지 궁금할 수 있습니다. 커맨드라인 인자는 프로그램에 매우 유용하고 유연한 입력 값을 제공하며, 이를 통해 여러분은 여러분의 프로그램을 사용자 정의할 수 있습니다. 이 블로그 포스트에서는 Elm 언어에서 커맨드라인 인자를 읽는 방법과 그 이유에 대해 살펴보겠습니다.

## 어떻게

커맨드라인 인자를 읽기 위해 먼저 `Platform.Cmd` 라이브러리를 임포트해야 합니다. 그런 다음 `Platform.Cmd.args` 함수를 호출하여 커맨드라인 인자를 읽을 수 있습니다.

```Elm
import Platform.Cmd exposing (args)

main =
  args \ arguments ->
    -- arguments 변수에 커맨드라인 인자가 저장됩니다.
    -- 여러분은 이 인자를 원하는대로 가공하여 사용할 수 있습니다.
```

위 코드에서, `args` 함수는 `List String`의 커맨드라인 인자를 받아오고, 그것을 함수 인자로 사용하여 여러분이 원하는대로 처리할 수 있도록 합니다. 이제 몇 가지 예제를 살펴보겠습니다.

```Elm
import Platform.Cmd exposing (args)

main =
  args \ arguments ->
    -- 인자를 그대로 출력합니다.
    print arguments

    -- 인자를 모두 대문자로 변환하여 출력합니다.
    print (List.map String.toUpper arguments)

    -- 두 번째 인자부터 마지막 인자까지 출력합니다.
    print (List.drop 1 arguments)
```

위 코드를 실행하면 커맨드 라인에서 다음과 같이 입력할 수 있습니다.

```
$ elm make Main.elm --debug --output=app.js
```

이를 처리하여 다음과 같은 결과가 출력됩니다.

```
> ["--debug", "--output=app.js"]
> ["--DEBUG", "--OUTPUT=APP.JS"]
> ["--output=app.js"]
```

위 예제는 여러분이 커맨드 라인 인자를 어떻게 원하는 대로 처리할 수 있는지 보여주는 것입니다.

## 깊게 파보기

이제 커맨드 라인 인자를 읽기 위해 `Platform.Cmd` 라이브러리를 사용하는 방법을 살펴보았는데, 여러분은 이것을 활용하여 여러분의 프로그램을 더욱 유연하게 만들 수 있습니다. 예를 들어, 여러분은 인자에 따라 다른 기능을 수행하는 프로그램을 만들 수 있을 뿐만 아니라, 인자 없이 실행할 경우 기본 설정값을 사용하는 등 다양한 방식으로 커맨드 라인 인자를 활용할 수 있습니다.

## 더 알아보기

여러분이 더 많은 정보를 원한다면, 다음 링크들을 참고해 보시기 바랍니다.

- [Elm Documentation - Platform Cmd](https://package.elm-lang.org/packages/elm/core/latest/Platform-Cmd)
- [Elm Tutorial - Command line arguments](https://elmprogramming.com/command-line-arguments.html)

## 참고

- [Elm 가이드 - Platform Cmd](https://package.elm-lang.org/packages/elm/core/latest/Platform-Cmd)
- [Elm 튜토리얼 - 커맨드 라인 인자](https://elmprogramming.com/command-line-arguments.html)