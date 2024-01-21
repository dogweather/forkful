---
title:                "명령줄 인수 읽기"
date:                  2024-01-20T17:56:18.155352-07:00
model:                 gpt-4-1106-preview
simple_title:         "명령줄 인수 읽기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (무엇 & 왜?)
커맨드 라인 인자 읽기는 프로그램이 시작할 때 사용자가 제공한 옵션과 값들을 처리하는 것입니다. 이를 통해 사용자가 다양한 상황에 맞춰 프로그램의 동작을 조정할 수 있기 때문에 중요합니다.

## How to: (어떻게:)
```gleam
import gleam/io
import gleam/list
import gleam/os

pub fn main(args: List(String)) {
  let cmd_args = os.args()
  
  match cmd_args {
    [] -> io.println("No arguments provided.")
    [first_arg | rest] -> 
      io.println("First argument: " ++ first_arg)
      io.println("Remaining arguments: " ++ list.join(rest, ", "))
  }
}

// 예상 출력:
// > my_program
// No arguments provided.
//
// > my_program arg1 arg2 arg3
// First argument: arg1
// Remaining arguments: arg2, arg3
```

## Deep Dive (심층 탐구)
예전에는 커맨드 라인 인자들을 직접 문자열로 파싱하는 경우가 많았습니다. 오늘날엔 대부분의 프로그래밍 언어들이 이러한 기능을 라이브러리로 제공하고 있죠. Gleam에서는 `gleam/os` 모듈이 이 일을 합니다. 인자들은 실행 파일 이름 다음에 공백으로 구분되어 전달되며, Gleam 리스트 형태로 접근할 수 있습니다.

대안으로는 커맨드 라인 파싱 라이브러리를 사용할 수 있는데, 이는 인자들을 조금 더 복잡하게 구성할 때 유용합니다. 하지만 간단한 사용사례에서는 Gleam의 기본 기능으로 충분합니다.

실행 중인 프로그램이 인자를 활용하기 위해, `os.args()` 를 호출하면, 현재 프로세스에 전달된 원시 인자들을 문자열 리스트로 반환받습니다. 그리고 패턴 매칭을 이용해 이 리스트를 다룰 수 있습니다.

## See Also (관련 자료)
- [Intro to Gleam (서론)](https://gleam.run/book/)

이 글을 통해 당신은 Gleam에서의 커맨드 라인 인자 읽기와 그 사용법, 그리고 이것이 어떻게 발전해 왔는지에 대해 간략히 알게 되었습니다. 관련 자료를 통해 Gleam에 대해 더 배우는 것도 잊지 마세요!