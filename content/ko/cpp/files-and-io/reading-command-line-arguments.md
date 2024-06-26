---
date: 2024-01-20 17:55:42.179585-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) \uCEE4\uB9E8\uB4DC\
  \ \uB77C\uC778 \uC778\uC218\uB294 UNIX\uC758 \uCD08\uCC3D\uAE30\uBD80\uD130 \uC0AC\
  \uC6A9\uB418\uC5C8\uC2B5\uB2C8\uB2E4. \uC0AC\uC6A9\uC790\uB294 \uD130\uBBF8\uB110\
  \uC744 \uD1B5\uD574 \uC785\uB825\uD558\uACE0 \uD504\uB85C\uADF8\uB7A8\uC740 `main`\
  \ \uD568\uC218\uC758 \uC778\uC218\uB97C \uD1B5\uD574 \uC774\uB97C \uBC1B\uC2B5\uB2C8\
  \uB2E4: `argc`\uB294 \uC778\uC218\uC758 \uC218\uB97C \uB098\uD0C0\uB0B4\uACE0, `argv`\uB294\
  \ \uC778\uC218 \uB0B4\uC6A9\uC744 \uB2F4\uACE0 \uC788\uB294 \uBB38\uC790\uC5F4 \uBC30\
  \uC5F4\uC785\uB2C8\uB2E4.\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:09.937123-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) \uCEE4\uB9E8\uB4DC \uB77C\uC778\
  \ \uC778\uC218\uB294 UNIX\uC758 \uCD08\uCC3D\uAE30\uBD80\uD130 \uC0AC\uC6A9\uB418\
  \uC5C8\uC2B5\uB2C8\uB2E4."
title: "\uBA85\uB839\uC904 \uC778\uC218 \uC77D\uAE30"
weight: 23
---

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
