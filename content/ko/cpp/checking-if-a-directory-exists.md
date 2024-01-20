---
title:                "디렉토리가 존재하는지 확인하기"
html_title:           "C++: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 무엇과 왜?

디렉토리가 존재하는지 확인(Check if a directory exists)이란, 어떤 디렉토리 경로가 실제로 존재하는지를 판단하는 작업을 의미합니다. 이는 우리가 파일을 읽거나 쓰려고 할 때 해당 위치가 유효한지를 미리 확인함으로써 에러를 방지하기 위해 필요한 작업입니다.

## 어떻게:

다음 코드는 디렉토리의 존재 여부를 확인하는 기본 예시입니다:

```C++
#include <filesystem>
#include <iostream>

namespace fs = std::filesystem;

int main() {
    if(fs::exists("mydir")) {
        std::cout << "Directory exists.\n";
    } else {
        std::cout << "Directory does not exist.\n";
    }

    return 0;
}
```

이 프로그램을 실행하면 "mydir"이라는 이름의 디렉토리가 존재하는지에 따라 다른 메시지가 출력됩니다.

## 딥 다이브:

디렉토리 존재 확인 기능은 파일 시스템에 관한 프로그래밍에서 핵심적인 역할을 합니다. 이 기능은 C++17에서 도입된 `<filesystem>` 라이브러리를 통해 간단하게 사용할 수 있습니다. 대안으로는 POSIX 기반 시스템에서 `stat` 함수를 사용할 수 있으나, C++ `<filesystem>`의 `exists` 함수는 플랫폼에 독립적이라는 장점이 있습니다.

## 추가자료:

다음은 디렉토리 존재 확인과 관련한 추가적인 정보를 얻을 수 있는 곳입니다:

1. [C++17 Filesystem Library - cppreference.com](https://en.cppreference.com/w/cpp/filesystem)
2. [How to check if a directory exists in C++ - StackOverflow](https://stackoverflow.com/questions/8233842/how-to-check-if-directory-exist-using-c-and-winapi)
3. [C++ Tutorial: File I/O - 2021 - bogotobogo](https://www.bogotobogo.com/cplusplus/files.php)