---
title:                "새 프로젝트 시작하기"
date:                  2024-01-20T18:03:05.465061-07:00
model:                 gpt-4-1106-preview
simple_title:         "새 프로젝트 시작하기"
programming_language: "C"
category:             "C"
tag:                  "Getting Started"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
새 프로젝트 시작은 공백에서 새로운 코드 세계로 나아가는 것입니다. 프로그래머들은 새로운 문제를 해결하거나 아이디어를 실현하기 위해 이 과정을 반복합니다.

## How to: (어떻게하다)
새 C 프로그램을 시작하려면 기본적인 구조가 필요합니다. 아래는 간단한 "Hello, World!" 프로그램입니다.

```c
#include <stdio.h>

int main() {
    printf("Hello, World!\n");
    return 0;
}
```

컴파일과 실행 결과:

```bash
$ gcc hello.c -o hello
$ ./hello
Hello, World!
```

## Deep Dive (심층 분석)
초기 C 프로그램은 1972년에 존재했습니다. C는 시스템과 응용 프로그래밍에서 널리 쓰입니다. 대안 언어로는 Python, Java, Rust 등이 있으나, C는 여전히 하드웨어에 가까운 프로그램이 필요할 때 많이 사용됩니다. 새 프로젝트를 시작할 때는 `main()` 함수가 진입점(entry point)입니다. 표준 라이브러리와 각종 라이브러리를 포함해 필요한 기능을 구현합니다.

## See Also (더 보기)
- [Learn C Programming](https://www.learn-c.org/)
- [GCC, the GNU Compiler Collection](https://gcc.gnu.org/)
- [C Standard Library Reference](https://en.cppreference.com/w/c/header)
