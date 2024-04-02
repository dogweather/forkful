---
date: 2024-01-20 17:55:42.179585-07:00
description: "\uBA85\uB839\uC904 \uC778\uC218\uB97C \uC77D\uB294 \uAC83\uC740 \uD504\
  \uB85C\uADF8\uB7A8 \uC2E4\uD589 \uC2DC \uC0AC\uC6A9\uC790\uAC00 \uC81C\uACF5\uD55C\
  \ \uC635\uC158\uC744 \uAC00\uC838\uC624\uB294 \uBC29\uBC95\uC785\uB2C8\uB2E4. \uD504\
  \uB85C\uADF8\uB798\uBA38\uB294 \uC77C\uBC18\uC801\uC73C\uB85C \uD504\uB85C\uADF8\
  \uB7A8\uC758 \uB3D9\uC791\uC744 \uC0AC\uC6A9\uC790\uAC00 \uC6D0\uD558\uB294 \uB300\
  \uB85C \uC870\uC815\uD558\uAE30 \uC704\uD574 \uC774\uB97C \uC218\uD589\uD569\uB2C8\
  \uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.691626-06:00'
model: gpt-4-1106-preview
summary: "\uBA85\uB839\uC904 \uC778\uC218\uB97C \uC77D\uB294 \uAC83\uC740 \uD504\uB85C\
  \uADF8\uB7A8 \uC2E4\uD589 \uC2DC \uC0AC\uC6A9\uC790\uAC00 \uC81C\uACF5\uD55C \uC635\
  \uC158\uC744 \uAC00\uC838\uC624\uB294 \uBC29\uBC95\uC785\uB2C8\uB2E4. \uD504\uB85C\
  \uADF8\uB798\uBA38\uB294 \uC77C\uBC18\uC801\uC73C\uB85C \uD504\uB85C\uADF8\uB7A8\
  \uC758 \uB3D9\uC791\uC744 \uC0AC\uC6A9\uC790\uAC00 \uC6D0\uD558\uB294 \uB300\uB85C\
  \ \uC870\uC815\uD558\uAE30 \uC704\uD574 \uC774\uB97C \uC218\uD589\uD569\uB2C8\uB2E4\
  ."
title: "\uBA85\uB839\uC904 \uC778\uC218 \uC77D\uAE30"
weight: 23
---

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
