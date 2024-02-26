---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:10.072343-07:00
description: "\uB514\uB809\uD1A0\uB9AC\uAC00 \uC874\uC7AC\uD558\uB294\uC9C0 \uD655\
  \uC778\uD55C\uB2E4\uB294 \uAC83\uC740, \uD30C\uC77C\uC744 \uC77D\uAC70\uB098 \uC4F0\
  \uAE30 \uC804\uC5D0 \uC9C0\uC815\uB41C \uACBD\uB85C\uC5D0 \uB514\uB809\uD1A0\uB9AC\
  \uAC00 \uC788\uB294\uC9C0\uB97C \uACB0\uC815\uD558\uB294 \uAC83\uC785\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uD30C\uC77C \uC791\uC5C5\uACFC \uAD00\
  \uB828\uB41C \uC624\uB958\uB97C \uD53C\uD558\uACE0, \uC790\uC2E0\uB4E4\uC758 \uC560\
  \uD50C\uB9AC\uCF00\uC774\uC158\uC5D0\uC11C \uD30C\uC77C \uCC98\uB9AC \uC791\uC5C5\
  \uC758 \uC2E4\uD589\uC744 \uB354\uC6B1 \uC6D0\uD65C\uD558\uACE0 \uC2E0\uB8B0\uD560\
  \ \uC218 \uC788\uAC8C \uB9CC\uB4E4\uAE30 \uC704\uD574 \uC774\uB7F0 \uC791\uC5C5\uC744\
  \u2026"
lastmod: '2024-02-25T18:49:52.674399-07:00'
model: gpt-4-0125-preview
summary: "\uB514\uB809\uD1A0\uB9AC\uAC00 \uC874\uC7AC\uD558\uB294\uC9C0 \uD655\uC778\
  \uD55C\uB2E4\uB294 \uAC83\uC740, \uD30C\uC77C\uC744 \uC77D\uAC70\uB098 \uC4F0\uAE30\
  \ \uC804\uC5D0 \uC9C0\uC815\uB41C \uACBD\uB85C\uC5D0 \uB514\uB809\uD1A0\uB9AC\uAC00\
  \ \uC788\uB294\uC9C0\uB97C \uACB0\uC815\uD558\uB294 \uAC83\uC785\uB2C8\uB2E4. \uD504\
  \uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uD30C\uC77C \uC791\uC5C5\uACFC \uAD00\uB828\
  \uB41C \uC624\uB958\uB97C \uD53C\uD558\uACE0, \uC790\uC2E0\uB4E4\uC758 \uC560\uD50C\
  \uB9AC\uCF00\uC774\uC158\uC5D0\uC11C \uD30C\uC77C \uCC98\uB9AC \uC791\uC5C5\uC758\
  \ \uC2E4\uD589\uC744 \uB354\uC6B1 \uC6D0\uD65C\uD558\uACE0 \uC2E0\uB8B0\uD560 \uC218\
  \ \uC788\uAC8C \uB9CC\uB4E4\uAE30 \uC704\uD574 \uC774\uB7F0 \uC791\uC5C5\uC744\u2026"
title: "\uB514\uB809\uD1A0\uB9AC\uAC00 \uC874\uC7AC\uD558\uB294\uC9C0 \uD655\uC778\
  \uD558\uAE30"
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
