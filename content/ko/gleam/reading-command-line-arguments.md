---
title:                "컴퓨터 프로그래밍에서의 명령 줄 인수 읽기"
html_title:           "Gleam: 컴퓨터 프로그래밍에서의 명령 줄 인수 읽기"
simple_title:         "컴퓨터 프로그래밍에서의 명령 줄 인수 읽기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

제목: Gleam으로 커맨드라인 인자 읽기

## 무엇이고 왜 사용하는가?

커맨드라인 인자를 읽는 것은 프로그래머가 코드를 실행할 때 입력받은 명령행 인수를 읽어오는 것을 말합니다. 올바른 입력을 확인하고, 알맞은 기능을 수행하는 데에 필수적입니다.

## 어떻게 하는가?

```Gleam
pub fn main(args) {
  show(args)
}
```

위의 코드는 Gleam에서 커맨드라인 인자를 읽는 가장 간단한 방법입니다. 이 코드를 실행하면, 입력받은 모든 인자를 보여주는 결과를 볼 수 있습니다. 예를 들어, `main("hello", "world")`를 실행하면 "hello"와 "world"가 출력됩니다.

## 딥 다이브

### 역사적 배경

커맨드라인 인자를 읽는 기능은 초기 컴퓨터 시스템에서부터 사용되어 왔습니다. 당시에는 커맨드라인 인자를 읽는 대신 프로그램 내부에서 하드코딩되어야 했지만, 지금은 커맨드라인 인자를 이용해 프로그램을 더 유연하고 다양한 상황에서 실행할 수 있게 되었습니다.

### 대안

커맨드라인 인자를 읽는 기능은 다른 언어에서도 지원됩니다. 대표적으로 C 언어는 `argc`와 `argv`라는 변수를 이용해 커맨드라인 인자를 읽습니다.

### 구현 세부 정보

Gleam에서는 `std/os` 라이브러리의 `args` 함수를 통해 커맨드라인 인자를 읽을 수 있습니다. 이 함수는 `List(String)` 타입의 값으로 인자를 돌려줍니다.

## 참고 자료

- [Gleam 문서 - Command Line Arguments](https://gleam.run/documentation/command_line_arguments)
- [C 커맨드라인 인자 읽기](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)
- [C++ 커맨드라인 인자 읽기](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)
- [Java 커맨드라인 인자 읽기](https://www.geeksforgeeks.org/command-line-arguments-in-java/)
- [Python 커맨드라인 인자 읽기](https://www.geeksforgeeks.org/command-line-arguments-in-python/)