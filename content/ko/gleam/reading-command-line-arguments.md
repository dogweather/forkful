---
title:                "컴퓨터 프로그래밍에서 명령 줄 인수 읽기"
html_title:           "Gleam: 컴퓨터 프로그래밍에서 명령 줄 인수 읽기"
simple_title:         "컴퓨터 프로그래밍에서 명령 줄 인수 읽기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜 읽어야 하는가?

커맨드 라인 인자를 읽는 것은 프로그래밍을 할 때 매우 유용합니다. 프로그램이 실행될 때 사용자로부터 입력을 받을 수 있고, 이를 활용하여 다양한 기능을 추가할 수 있습니다.

## 어떻게 읽어야 하나요?

커맨드 라인 인자를 읽는 방법은 매우 간단합니다. 아래 코드 블록을 참고하여 Gleam에서 어떻게 커맨드 라인 인자를 읽을 수 있는지 알아보세요.

```Gleam
let command_line_args = command_line.args
```

위 코드는 "command_line.args"라는 함수를 사용하여 커맨드 라인 인자를 읽어옵니다. 이후 변수에 저장하면, 해당 변수를 사용하여 인자를 활용할 수 있습니다.

아래는 간단한 예제 코드입니다.

```Gleam
let command_line_args = command_line.args

fn main() {
  case command_line_args {
    [] -> println("No arguments provided.")
    [arg] -> println("The first argument is: ${arg}")
    [arg1, arg2] -> println("The second argument is: ${arg2}")
  }
}
```

위 코드는 커맨드 라인 인자가 없을 때는 "No arguments provided."라는 메시지를 출력하고, 하나의 인자가 있을 때는 "The first argument is: ${arg}" 형식으로 첫 번째 인자를 출력합니다. 두 개의 인자가 있을 때는 두 번째 인자를 출력하게 됩니다.

## 더 알아보기

커맨드 라인 인자를 읽는 것은 매우 간단한 작업이지만, 좀 더 깊이 들어가면 더 많은 기능을 추가할 수 있습니다. 예를 들어, 인자의 타입이나 순서에 따라 다른 동작을 구현하거나, 정해진 형식에 맞는 인자만 받아서 처리하는 등의 기능을 추가할 수 있습니다.

## 더 알아보기

- Gleam 공식 문서: https://gleam.run/
- Gleam GitHub 저장소: https://github.com/gleam-lang/gleam