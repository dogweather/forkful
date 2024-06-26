---
date: 2024-01-20 17:53:55.773382-07:00
description: "How to: (\uBC29\uBC95) \uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC77D\uAE30\uB294\
  \ \uD504\uB85C\uADF8\uB798\uBC0D \uCD08\uAE30\uBD80\uD130 \uC911\uC694\uD55C \uBD80\
  \uBD84\uC774\uC5C8\uC2B5\uB2C8\uB2E4. C++\uC5D0\uC11C\uB294 `<fstream>` \uD5E4\uB354\
  \uC5D0 \uC815\uC758\uB41C `ifstream` \uD074\uB798\uC2A4\uB97C \uC0AC\uC6A9\uD574\
  \ \uD30C\uC77C \uC77D\uAE30\uB97C \uC218\uD589\uD569\uB2C8\uB2E4. \uB300\uC548\uC73C\
  \uB85C `FILE*` \uD3EC\uC778\uD130\uC640 `fopen`, `fgets` \uB4F1\uC758 C \uC2A4\uD0C0\
  \uC77C\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:09.939301-06:00'
model: gpt-4-1106-preview
summary: "(\uBC29\uBC95) \uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC77D\uAE30\uB294 \uD504\
  \uB85C\uADF8\uB798\uBC0D \uCD08\uAE30\uBD80\uD130 \uC911\uC694\uD55C \uBD80\uBD84\
  \uC774\uC5C8\uC2B5\uB2C8\uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC77D\uAE30"
weight: 22
---

## How to: (방법)
```C++
#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ifstream file("example.txt"); // 텍스트 파일 열기

    if (file.is_open()) {
        std::string line;
        while (getline(file, line)) { // 파일 끝까지 한 줄씩 읽기
            std::cout << line << '\n'; // 읽은 줄 출력
        }
        file.close(); // 파일 닫기
    } else {
        std::cout << "파일을 열 수 없습니다." << std::endl;
    }

    return 0;
}
```
Sample Output:
```
첫 번째 줄입니다.
두 번째 줄입니다.
세 번째 줄입니다.
```

## Deep Dive (심도 있는 탐구)
텍스트 파일 읽기는 프로그래밍 초기부터 중요한 부분이었습니다. C++에서는 `<fstream>` 헤더에 정의된 `ifstream` 클래스를 사용해 파일 읽기를 수행합니다. 대안으로 `FILE*` 포인터와 `fopen`, `fgets` 등의 C 스타일 함수도 사용 가능하지만, C++은 `ifstream`을 사용하는 것이 더 안전하고 간편합니다. 파일을 읽을 때는 항상 파일이 제대로 열렸는지 확인하고, 예외 케이스를 처리해주어야 합니다. 또한, 파일을 사용한 후에는 반드시 닫아주는 것이 좋습니다. C++11부터는 RAII(Resource Acquisition Is Initialization) 패턴을 이용해 파일을 자동으로 닫을 수 있도록 `std::ifstream`이 개선되었습니다.

## See Also (더 보기)
- [C++ 파일 입출력 (cppreference.com)](https://en.cppreference.com/w/cpp/io)
- [Learn C++ File Handling (tutorialspoint.com)](https://www.tutorialspoint.com/cplusplus/cpp_files_streams.htm)
- [C++ 입출력 라이브러리 (cplusplus.com)](http://www.cplusplus.com/reference/fstream/ifstream/)
