---
title:                "디렉토리의 존재 여부 확인하기"
html_title:           "Arduino: 디렉토리의 존재 여부 확인하기"
simple_title:         "디렉토리의 존재 여부 확인하기"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
`directory` 존재 여부를 확인하는 것은 파일 시스템에 특정 폴더가 있는지 확인하는 과정이다. 프로그래머는 파일을 저장하거나 읽기 전 유효한 경로인지 확인하기 위해 이 작업을 수행한다.

## How to: (어떻게 하나요?)
C++17부터 지원하는 `<filesystem>` 헤더를 사용해서 `directory` 존재 여부를 간단하게 확인할 수 있다. 다음은 예제 코드와 그 출력 결과이다.

```C++
#include <iostream>
#include <filesystem>

namespace fs = std::filesystem;

int main() {
    fs::path dir_to_check = "/path/to/directory";

    if (fs::exists(dir_to_check)) {
        std::cout << "The directory exists.\n";
    } else {
        std::cout << "The directory does not exist.\n";
    }

    return 0;
}
```

예제 출력 결과는 다음과 같다:

```
The directory exists.
```
또는 
```
The directory does not exist.
```

## Deep Dive (심층 분석)
과거에는 C++ 표준에는 디렉토리 존재 여부를 직접적으로 확인할 수 있는 기능이 없었다. 대신, `boost::filesystem` 라이브러리가 사용되곤 했다. C++17 이전에는 `stat` 함수(POSIX API)를 사용하거나 플랫폼 종속적인 API를 이용해야 했다.

`<filesystem>`은 파일 시스템을 다루는 표준이며, `fs::exists` 함수는 말 그대로 파일이나 디렉토리가 존재하는지 확인한다. 기존에는 직접 OS에 따른 API를 호출하거나 외부 라이브러리에 의존해야 했던 작업을, 이제는 훨씬 간단하게 할 수 있게 되었다. 또한 에러 처리도 간결해졌다. `fs::exists`는 `fs::filesystem_error`를 던지지 않고 실패 시에는 단지 `false`를 반환한다.

## See Also (참고 자료)
- [cppreference.com on std::filesystem](https://en.cppreference.com/w/cpp/filesystem)
- Boost Filesystem (이용하고자 한다면, C++17 이전에 대한 읽어볼 만한 내용): [Boost Filesystem Documentation](https://www.boost.org/doc/libs/1_75_0/libs/filesystem/doc/index.htm)
- POSIX 'stat' function documentation for comparison: [POSIX stat](https://pubs.opengroup.org/onlinepubs/009695399/functions/stat.html)