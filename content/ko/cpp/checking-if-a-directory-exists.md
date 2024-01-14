---
title:                "C++: 디렉터리가 존재하는지 확인하는 것"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜

디렉토리가 있는지 확인하는 것이 왜 중요한지 아시나요? 만약 당신이 프로그램을 작성하거나 파일을 관리한다면, 그리고 파일 시스템에 대한 이해가 있다면 이 작업은 필수적입니다. 디렉토리 존재 여부를 확인함으로써 당신은 앞으로의 작업에 대해 확실한 기반이 마련될 것입니다.

## 어떻게

```C++
#include <iostream>
#include <filesystem>

using namespace std;
namespace fs = std::filesystem;

int main() {
    if (fs::is_directory("my_directory") == true) {
        cout << "디렉토리가 존재합니다." << endl;
    } else {
        cout << "디렉토리는 존재하지 않습니다." << endl;
    }
    
    return 0;
}
```

위의 예제 코드에서 우리는 `<filesystem>` 라이브러리를 사용하여 디렉토리의 존재 여부를 확인했습니다. 이 라이브러리는 C++17부터 도입되었기 때문에 해당 버전을 지원하는 컴파일러를 사용해야 합니다. 디렉토리가 존재하는 경우 `is_directory()` 함수는 `true`를 반환하고, 그렇지 않은 경우 `false`를 반환합니다.

## 딥 다이브

C++17이 아니라면 어떻게 디렉토리 존재 여부를 확인할 수 있을까요? 이전에는 `<filesystem>` 라이브러리가 없기 때문에 C++ 표준 라이브러리에서 디렉토리 관리 기능을 제공하지 않았습니다. 그래서 우리는 시스템 종속적인 함수를 사용해야 했습니다. 예를 들어 윈도우에서는 `GetFileAttributes()` 함수를 사용하여 파일의 속성을 확인하고, 리눅스에서는 `stat()` 함수를 사용하면 됩니다.

하지만 C++17 이후에는 `<filesystem>` 라이브러리를 통해 시스템 종속적인 함수 없이도 디렉토리를 관리할 수 있게 되었습니다. 이를 통해 코드의 포팅이 쉬워졌고, 플랫폼간의 호환성 또한 높아졌습니다.

## 더 알아보기

- [`<filesystem>` 라이브러리 공식 문서 (영문)](https://en.cppreference.com/w/cpp/filesystem)
- [C++17에서 파일시스템 수정 (한글 블로그)](https://m.blog.naver.com/PostView.nhn?blogId=roberet&logNo=221471189225&categoryNo=21&proxyReferer=https:%2F%2Fwww.google.com%2F)
- [윈도우에서 디렉토리 존재 여부 확인하는 방법 (한글 블로그)](https://velog.io/@moongq/how-to-check-the-directory-exist-in-windows-using-cpp)