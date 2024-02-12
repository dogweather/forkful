---
title:                "명령줄 인수 읽기"
aliases:
- /ko/cpp/reading-command-line-arguments.md
date:                  2024-01-20T17:55:42.179585-07:00
model:                 gpt-4-1106-preview
simple_title:         "명령줄 인수 읽기"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜?)
명령줄 인수를 읽는 것은 프로그램 실행 시 사용자가 제공한 옵션을 가져오는 방법입니다. 프로그래머는 일반적으로 프로그램의 동작을 사용자가 원하는 대로 조정하기 위해 이를 수행합니다.

## How to: (어떻게 하나요?)
```C++
#include <iostream>

int main(int argc, char* argv[]){
    std::cout << "Program Name: " << argv[0] << std::endl;
    for(int i = 1; i < argc; ++i) {
        std::cout << "Argument " << i << ": " << argv[i] << std::endl;
    }
    return 0;
}
```
**실행 예:**
```
$ ./myprogram option1 value1
Program Name: ./myprogram
Argument 1: option1
Argument 2: value1
```

## Deep Dive (깊은 탐구)
커맨드 라인 인수는 UNIX의 초창기부터 사용되었습니다. 사용자는 터미널을 통해 입력하고 프로그램은 `main` 함수의 인수를 통해 이를 받습니다: `argc`는 인수의 수를 나타내고, `argv`는 인수 내용을 담고 있는 문자열 배열입니다. 대안으로 `getopt` 라이브러리나 C++17의 `std::filesystem`이 있지만, 가볍고 간단한 요구 사항에는 전통적인 방식이 여전히 흔히 쓰입니다. 구현은 시스템마다 다를 수 있으니 portable code 작성 시 주의가 필요합니다.

## See Also (더 보기)
- [cppreference.com: Main function](https://en.cppreference.com/w/cpp/language/main_function)
- [GNU: Using the getopt library](https://www.gnu.org/software/libc/manual/html_node/Getopt.html)
- [cplusplus.com: Argc and argv](http://www.cplusplus.com/articles/DEN36Up4/)
