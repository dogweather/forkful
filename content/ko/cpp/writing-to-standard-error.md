---
title:                "표준 오류로 쓰기"
date:                  2024-01-19
html_title:           "Bash: 표준 오류로 쓰기"
simple_title:         "표준 오류로 쓰기"

category:             "C++"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜 사용할까?)
표준 에러는 프로그램 실행 도중 발생하는 오류 메시지를 출력하기 위해 사용되는 출력 스트림입니다. 프로그래머들은 오류를 다른 출력과 분리하여 더 쉽게 문제를 진단하고 로깅하기 위해 이를 사용합니다.

## How to: (어떻게 사용할까?)
```cpp
#include <iostream>

int main() {
    // 정상 출력
    std::cout << "Hello, World!" << std::endl;

    // 에러 출력
    std::cerr << "An error has occurred!" << std::endl;

    return 0;
}
```
**샘플 출력**
```
Hello, World!
An error has occurred!
```

## Deep Dive (심층 분석)
표준 에러(`stderr`)는 UNIX 시스템의 초기부터 사용되어오고 있습니다. `stdout`과 구분하여 사용함으로써 오류 메시지와 일반 출력을 분리할 수 있어 유용합니다. 대안으로는 파일로 로깅을 하거나 시스템 로그를 사용하는 방법이 있습니다. C++에서 `std::cerr` 는 내부적으로 `std::ostream` 클래스의 인스턴스로 구현되어 있으며 버퍼링되지 않아 즉시 출력됩니다.

## See Also (관련 자료)
- [cppreference.com: std::cerr](https://en.cppreference.com/w/cpp/io/cerr)
- [cppreference.com: I/O Stream Library](https://en.cppreference.com/w/cpp/io)
- [GNU C Library: Standard Streams](https://www.gnu.org/software/libc/manual/html_node/Standard-Streams.html)
