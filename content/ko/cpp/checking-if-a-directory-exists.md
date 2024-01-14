---
title:    "C++: 디렉토리가 존재하는지 확인하기"
keywords: ["C++"]
---

{{< edit_this_page >}}

## 왜 디렉토리가 존재하는지 확인하는 가요?

디렉토리가 존재하는지 확인하는 것은 프로그래밍에서 자주 사용되는 중요한 작업입니다. 이는 파일을 읽고 쓸 때 무결성을 보장하고, 중복 파일을 방지할 수 있으며, 파일이 존재하지 않을 때 오류를 방지할 수 있기 때문입니다.

## 어떻게 확인할 수 있나요?

디렉토리가 존재하는지 확인하는 가장 간단한 방법은 `std::experimental::filesystem` 라이브러리에서 제공하는 `std::experimental::filesystem::exists()` 함수를 사용하는 것입니다. 이 함수는 주어진 경로가 존재하는지 아닌지를 `true` 또는 `false`로 반환합니다.

```C++
#include <iostream>
#include <experimental/filesystem>

namespace fs = std::experimental::filesystem;

int main() {
    fs::path directory_path = "C:/Users/User/Desktop/NewFolder";

    // 디렉토리가 존재하는지 확인
    if (fs::exists(directory_path)) {
        std::cout << "디렉토리가 존재합니다.";
    }
    else {
        std::cout << "디렉토리가 존재하지 않습니다.";
    }

    return 0;
}
```

위 코드의 출력은 "디렉토리가 존재합니다." 또는 "디렉토리가 존재하지 않습니다."가 됩니다.

## 깊게 들어가기

위의 예제 코드에서는 `std::experimental::filesystem::exists()` 함수를 사용했지만, 이 함수는 실험적인 기능이므로 이를 대체할 다른 함수를 사용하는 것이 좋습니다. 예를 들어, `std::filesystem::exists()` 함수는 이미 C++17 표준으로 포함되어 있습니다.

디렉토리가 존재하는지 확인하는 것은 실제로 `std::filesystem::exists()` 함수의 내부에서 `stat()` 시스템 호출을 통해 이루어집니다. 따라서 이 함수는 범용적으로 사용되는 함수이며, 여러 가지 파일 시스템에서 동작하는 데에 사용될 수 있습니다.

## 관련 자료

* [C++ reference - std::experimental::filesystem::exists](https://en.cppreference.com/w/cpp/filesystem/exists)
* [C++ reference - std::filesystem::exists](https://en.cppreference.com/w/cpp/filesystem/exists)
* [C++ reference - stat() function](https://en.cppreference.com/w/cpp/io/c/stat)