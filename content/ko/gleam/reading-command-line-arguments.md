---
title:                "Gleam: 컴퓨터 프로그래밍에서 명령줄 인수 읽기"
simple_title:         "컴퓨터 프로그래밍에서 명령줄 인수 읽기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

Gleam 프로그래밍 블로그 포스트 - 커맨드 라인 인자를 읽는 방법

## 왜?
커맨드 라인 인자를 읽는 것은 프로그램 실행 시 사용자에게 선택권을 주는 중요한 요소입니다. 이를 통해 사용자는 프로그램이 어떻게 동작하는지에 대한 조절을 할 수 있고, 프로그램의 실행을 더욱 유연하게 커스터마이즈할 수 있습니다.

## 어떻게?
아래의 코드 예시를 통해 Gleam에서 커맨드 라인 인자를 읽는 방법을 살펴보세요.

```Gleam
extern fn main(args) {
    case args {
        [] -> println("인자 없음")
        [a] -> println("하나의 인자만 사용: {}", a)
        [a, b] -> println("두 개의 인자 사용: {} {}", a, b)
        [a, ..rest] -> println("하나 이상의 인자 사용: {} {}", a, rest)
    }
}
```

위 예시에서는 `args`라는 이름의 매개변수를 사용하여 커맨드 라인 인자를 받아와서 처리하는 방법을 알려주고 있습니다. `fn` 키워드를 사용하여 외부 함수를 정의하고, `main`이라는 함수 이름을 지정하여 커맨드 라인 인자를 받아오고 처리하는 코드를 작성하고 있습니다. `fn` 키워드 뒤에 오는 `main` 함수의 괄호 안에는 `args`라는 매개변수가 지정되어 있습니다. 이는 받아온 커맨드 라인 인자를 담을 변수입니다.

위 코드 예시에서는 `case`를 통해 여러 경우에 대한 처리 방법이 지정되어 있습니다. 인자가 없는 경우에는 `"인자 없음"`이라는 문자열을 출력하고, 하나의 인자만 있는 경우에는 해당 인자를 출력하고 있습니다. 또한 두 개의 인자가 있는 경우와 하나 이상의 인자가 있는 경우에 대해서도 각각의 상황에 맞는 처리 방법을 지정하고 있습니다.

이처럼 `case`를 사용하여 여러 경우의 수를 고려하여 커맨드 라인 인자를 처리할 수 있습니다.

## 딥 다이브
커맨드 라인 인자를 처리하는 더 깊은 내용을 알고 싶다면, Gleam 공식 문서에서 제공하는 [커맨드 라인 인자 읽기](https://gleam.run/book/tour/command_line_arguments.html) 페이지를 참고하세요. 이 페이지에서는 Gleam에서 커맨드 라인 인자를 어떻게 읽고 처리하는지에 대한 더 자세한 설명과 예시 코드를 제공하고 있습니다.

## 이외의 정보
다른 Gleam 관련 정보를 알고 싶다면, 아래의 링크들을 참고해보세요.

- [Gleam 공식 홈페이지](https://gleam.run)
- [Gleam 공식 문서](https://gleam.run/book)
- [Gleam GitHub 저장소](https://github.com/gleam-lang/gleam)