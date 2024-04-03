---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:10.072343-07:00
description: "\uBC29\uBC95: \uD604\uB300\uC758 C++(C++17 \uC774\uD6C4)\uC5D0\uC11C\
  \uB294 \uD30C\uC77C\uC2DC\uC2A4\uD15C \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uC0AC\
  \uC6A9\uD558\uC5EC \uB514\uB809\uD1A0\uB9AC\uC758 \uC874\uC7AC \uC5EC\uBD80\uB97C\
  \ \uD655\uC778\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uC774 \uB77C\uC774\uBE0C\uB7EC\
  \uB9AC\uB294 \uB514\uB809\uD1A0\uB9AC\uC758 \uC874\uC7AC \uC5EC\uBD80\uB97C \uD655\
  \uC778\uD558\uB294 \uAC83\uC744 \uD3EC\uD568\uD558\uC5EC \uD30C\uC77C\uC2DC\uC2A4\
  \uD15C \uC791\uC5C5\uC744 \uC218\uD589\uD558\uB294 \uD45C\uC900\uD654\uB418\uACE0\
  \ \uC9C1\uAD00\uC801\uC778 \uBC29\uBC95\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.690223-06:00'
model: gpt-4-0125-preview
summary: "\uD604\uB300\uC758 C++(C++17 \uC774\uD6C4)\uC5D0\uC11C\uB294 \uD30C\uC77C\
  \uC2DC\uC2A4\uD15C \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uC0AC\uC6A9\uD558\uC5EC\
  \ \uB514\uB809\uD1A0\uB9AC\uC758 \uC874\uC7AC \uC5EC\uBD80\uB97C \uD655\uC778\uD560\
  \ \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uB514\uB809\uD1A0\uB9AC\uAC00 \uC874\uC7AC\uD558\uB294\uC9C0 \uD655\uC778\
  \uD558\uAE30"
weight: 20
---

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
