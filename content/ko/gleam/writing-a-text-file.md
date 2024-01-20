---
title:                "텍스트 파일 작성하기"
html_title:           "Arduino: 텍스트 파일 작성하기"
simple_title:         "텍스트 파일 작성하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
텍스트 파일 작성은 문자 데이터를 파일로 저장 하는 것입니다. 프로그래머는 데이터 로그, 설정, 사용자 입력을 저장하기 위해 텍스트 파일을 작성합니다.

## How to: (어떻게 하나요?)
Gleam어에는 표준 라이브러리에서 파일을 쉽게 만들고 쓸 수 있는 기능이 있습니다. 예제 코드를 보겠습니다:

```gleam
import gleam/io

pub fn main() -> Result(Nil, String) {
  io.write(to: "./hello.txt", "안녕, 글리am!")
  .map(|_| Nil)
}
```

위의 코드를 실행하면, 현재 디렉토리에 "hello.txt" 파일이 생성되어 "안녕, 글리am!" 이라는 내용이 담기게 됩니다.

## Deep Dive (심화 학습)
텍스트 파일 작성은 오랜 기간 컴퓨터 과학에서 사용되어 왔으며, 다양한 프로그래밍 언어에서 지원합니다. Gleam어에서는 Erlang의 VM을 기반으로하며, 더 안전하고 간편한 API를 제공합니다. 파일 I/O(입출력)는 실패할 수 있기 때문에, Gleam은 이러한 연산을 `Result` 타입으로 처리하여 에러를 명시적으로 다룰 수 있게 합니다.

## See Also (관련 링크)
- Gleam 공식 문서: [https://gleam.run](https://gleam.run)
- Erlang 파일 I/O 도큐멘테이션: [https://erlang.org/doc/man/file.html](https://erlang.org/doc/man/file.html)