---
title:    "Elm: 명령 줄 인수 읽기"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

다양한 언어를 배우고 있는 개발자라면 새로운 도구로 적응하는 과정을 두려워하지 않는 것이 중요합니다. 그리고 가장 강력한 도구 중 하나인 Elm을 배우는 것은 더 큰 가능성을 가질 수 있다는 것을 여러분에게 알려드립니다. 이번 포스트에서는 Elm으로 command line arguments를 읽는 방법에 대해 알아보겠습니다.

## 어떻게

우선, Elm으로 코드를 작성하기 위해서는 Elm 설치가 필요합니다. 설치는 공식 사이트에서 제공하는 방법을 따라하면 쉽게 할 수 있습니다. 이후, command line arguments를 읽기 위해 `elm/cli`패키지를 사용할 수 있습니다. 아래 예제를 통해 코드와 출력을 확인해보세요.

```Elm
import Cli
import List exposing (head)

main : Cli.Flags () -> Platform.Program () () Msg
main flags =
  case Cli.arguments () of
    [first, second] ->
      -- 첫번째와 두번째 argument를 불러와 출력합니다.
      Cl