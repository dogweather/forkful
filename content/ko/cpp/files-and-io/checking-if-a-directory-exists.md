---
title:                "디렉토리가 존재하는지 확인하기"
date:                  2024-02-03T19:07:10.072343-07:00
model:                 gpt-4-0125-preview
simple_title:         "디렉토리가 존재하는지 확인하기"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇을, 왜?
디렉토리가 존재하는지 확인한다는 것은, 파일을 읽거나 쓰기 전에 지정된 경로에 디렉토리가 있는지를 결정하는 것입니다. 프로그래머들은 파일 작업과 관련된 오류를 피하고, 자신들의 애플리케이션에서 파일 처리 작업의 실행을 더욱 원활하고 신뢰할 수 있게 만들기 위해 이런 작업을 합니다.

## 방법:
현대의 C++(C++17 이후)에서는 파일시스템 라이브러리를 사용하여 디렉토리의 존재 여부를 확인할 수 있습니다. 이 라이브러리는 디렉토리의 존재 여부를 확인하는 것을 포함하여 파일시스템 작업을 수행하는 표준화되고 직관적인 방법을 제공합니다.

```cpp
#include <iostream>
#include <filesystem>

namespace fs = std::filesystem;

int main() {
    const fs::path dirPath = "/path/to/directory";

    if (fs::exists(dirPath) && fs::is_directory(dirPath)) {
        std::cout << "디렉토리가 존재합니다." << std::endl;
    } else {
        std::cout << "디렉토리가 존재하지 않습니다." << std::endl;
    }

    return 0;
}
```
디렉토리가 존재하는 경우 샘플 출력:
```
디렉토리가 존재합니다.
```

디렉토리가 존재하지 않는 경우 샘플 출력:
```
디렉토리가 존재하지 않습니다.
```

아직 C++17을 사용하지 않거나 추가 기능이 필요한 프로젝트의 경우, Boost 파일시스템 라이브러리는 유사한 기능을 제공하는 인기 있는 타사 선택입니다.

```cpp
#include <iostream>
#include <boost/filesystem.hpp>

namespace fs = boost::filesystem;

int main() {
    const fs::path dirPath = "/path/to/directory";

    if (fs::exists(dirPath) && fs::is_directory(dirPath)) {
        std::cout << "디렉토리가 존재합니다." << std::endl;
    } else {
        std::cout << "디렉토리가 존재하지 않습니다." << std::endl;
    }

    return 0;
}
```
Boost 파일시스템을 사용하면, 지정된 경로의 디렉토리 존재 여부에 따라 C++17 파일시스템 예제와 동일한 출력이 됩니다.
