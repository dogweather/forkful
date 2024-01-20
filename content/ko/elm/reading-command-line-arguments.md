---
title:                "명령줄 인수 읽기"
html_title:           "Arduino: 명령줄 인수 읽기"
simple_title:         "명령줄 인수 읽기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

명령행 인자 읽기는 프로그램이 터미널에서 그 다음에 오는 인자들을 받아오는 것을 의미합니다. 프로그래머들이 이것을 하는 이유는 사용자가 프로그램을 실행할 때 인자로 데이터를 제공하여 프로그램의 동작을 변경하거나 관리할 수 있게하기 위해서입니다. 

## 어떻게 할까:

Elm에서는 Marchelito 패키지를 사용하여 명령행 인자를 읽는다. 그 예는 다음과 같습니다:
```Elm
import Marchelito

main : Program () () ()
main =
    Marchelito.run () update view

update : CmdArgument -> () -> Tuple (CmdArgument, Cmd CmdArgument)
update arg () =
   Tuple (arg, Cmd.none)

view : () -> Html
view _ =
  div []
    [ text "Hello, world!" 
    , text (toString (Marchelito.args))
    ]
```

## 깊은 탐구:

명령행 인자를 다루는 작업은 개발의 역사가 거듭되며 이루어져 왔습니다. 소프트웨어를 사용자가 원하는 대로 제어할 수 있도록 해주는 간편한 방법 중 하나입니다. 

대안적으로, 다른 패키지나 도구를 사용하여 명령행 인자를 읽어 올 수 있습니다만, Elm에서 가장 일반적으로 사용하는 방법은 Marchelito 패키지를 사용하는 것입니다. 

Marchelito 패키지는 환경 변수를 읽어 오는 Code가 자동으로 생성되며, 이 코드는 Elm 애플리케이션의 main 함수로 전달됩니다. 

## 참조 자료:

2. [Elm official documentation](https://elm-lang.org/) 
4. [Elm guide on interactivity](https://guide.elm-lang.org/interop/)