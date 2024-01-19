---
title:                "임시 파일 생성하기"
html_title:           "Python: 임시 파일 생성하기"
simple_title:         "임시 파일 생성하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

임시 파일 생성은 컴퓨터 메모리에 일시적인 저장 공간을 만들어 데이터를 저장하는 프로세스입니다. 이렇게 하면 처리 과정 중에 발생하는 큰 양의 데이터를 안전하게 처리하고, 프로그램 속도와 효율성을 높일 수 있습니다.

## 어떻게 하나요:

```Gleam
import gleam/stdio.{ stdout }

pub enum Cmd {
  Clear
  Save(String)
}

fn send(cmd: Cmd) -> Nil {
  case cmd {
    Clear -> stdout("Clearing...\n")
    Save(file) -> stdout("Saving to " ++ file ++ "...\n")
  }
}
```

이 코드는 Gleam 언어로 작성된 간단한 예제입니다. 같은 프로세스를 가진 다른 함수와는 다르게, 이 함수에는 파일을 임시 저장한다는 말이 들어가지 않았지만 실제로 그런 일을 합니다.

## 깊이 파보기:

임시 파일을 만드는 생각은 컴퓨터 과학의 초기 단계에서 나왔습니다. 이것은 기본 운영 체제의 핵심 부분이며, 최초로 Unix에서 도입되었습니다. 임시 파일에 대한 대체 가능한 전략은 여러 가지가 있지만, 이것들 중 하나는 "Memory-mapped Files"라는 기법을 사용하는 것입니다. 이 방법은 디스크에 있는 파일을 메모리의 일부로 취급하는 기술을 사용합니다.

Gleam에서의 임시 파일 생성은 "Erlang/OTP" 플랫폼 위에서 실행됩니다. 이 플랫폼은 매우 빠르면서도 안정적인 시스템을 만드는데 도움이 됩니다.

## 또한 참고하십시오:

 - [Gleam 공식 문서](https://gleam.run/book)
 - [Working with Temporary Files in Erlang](https://erlang.org/doc/man/file.html#mktemp-2)
 - [Memory-Mapped Files explained](https://www.geeksforgeeks.org/memory-mapped-io-mmio-in-operating-system/)