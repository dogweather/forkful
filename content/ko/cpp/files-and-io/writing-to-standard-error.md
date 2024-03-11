---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:45.171292-07:00
description: "C++\uC5D0\uC11C \uD45C\uC900 \uC624\uB958(`stderr`)\uB85C \uC791\uC131\
  \uD558\uAE30\uB780 \uBA54\uC778 \uD504\uB85C\uADF8\uB7A8 \uCD9C\uB825\uACFC \uBD84\
  \uB9AC\uB41C \uC624\uB958 \uBA54\uC2DC\uC9C0\uB098 \uC9C4\uB2E8 \uC815\uBCF4\uB97C\
  \ \uCD9C\uB825\uD558\uB294 \uAC83\uC744 \uB9D0\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\
  \uB798\uBA38\uB4E4\uC740 \uC774\uB97C \uD1B5\uD574 \uC624\uB958\uB97C \uB2E4\uB978\
  \ \uC2A4\uD2B8\uB9BC\uC73C\uB85C \uC9C1\uC811 \uBCF4\uB0B4\uC5B4, \uC77C\uBC18 \uCD9C\
  \uB825\uACFC \uC624\uB958 \uBA54\uC2DC\uC9C0\uB97C \uAD6C\uBCC4\uD568\uC73C\uB85C\
  \uC368 \uB514\uBC84\uAE45\uACFC \uC624\uB958 \uCC98\uB9AC\uB97C \uC27D\uAC8C \uD560\
  \ \uC218 \uC788\uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-11T00:14:29.619131-06:00'
model: gpt-4-0125-preview
summary: "C++\uC5D0\uC11C \uD45C\uC900 \uC624\uB958(`stderr`)\uB85C \uC791\uC131\uD558\
  \uAE30\uB780 \uBA54\uC778 \uD504\uB85C\uADF8\uB7A8 \uCD9C\uB825\uACFC \uBD84\uB9AC\
  \uB41C \uC624\uB958 \uBA54\uC2DC\uC9C0\uB098 \uC9C4\uB2E8 \uC815\uBCF4\uB97C \uCD9C\
  \uB825\uD558\uB294 \uAC83\uC744 \uB9D0\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\
  \uBA38\uB4E4\uC740 \uC774\uB97C \uD1B5\uD574 \uC624\uB958\uB97C \uB2E4\uB978 \uC2A4\
  \uD2B8\uB9BC\uC73C\uB85C \uC9C1\uC811 \uBCF4\uB0B4\uC5B4, \uC77C\uBC18 \uCD9C\uB825\
  \uACFC \uC624\uB958 \uBA54\uC2DC\uC9C0\uB97C \uAD6C\uBCC4\uD568\uC73C\uB85C\uC368\
  \ \uB514\uBC84\uAE45\uACFC \uC624\uB958 \uCC98\uB9AC\uB97C \uC27D\uAC8C \uD560 \uC218\
  \ \uC788\uC2B5\uB2C8\uB2E4."
title: "\uD45C\uC900 \uC5D0\uB7EC\uC5D0 \uC4F0\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?

C++에서 표준 오류(`stderr`)로 작성하기란 메인 프로그램 출력과 분리된 오류 메시지나 진단 정보를 출력하는 것을 말합니다. 프로그래머들은 이를 통해 오류를 다른 스트림으로 직접 보내어, 일반 출력과 오류 메시지를 구별함으로써 디버깅과 오류 처리를 쉽게 할 수 있습니다.

## 방법:

C++에서 표준 오류에 작성하는 것은 표준 라이브러리의 일부인 `cerr` 스트림을 사용하여 달성할 수 있습니다. 기본 예시는 다음과 같습니다:

```cpp
#include <iostream>

int main() {
    // 표준 출력으로 작성
    std::cout << "이것은 일반 메시지입니다." << std::endl;
    
    // 표준 오류로 작성
    std::cerr << "이것은 오류 메시지입니다." << std::endl;
    
    return 0;
}
```

샘플 출력:
```
이것은 일반 메시지입니다.
이것은 오류 메시지입니다.
```

이 경우, 두 메시지 모두 당신의 터미널에 보통 나타납니다만, 셸에서 별도로 리디렉션할 수 있습니다. 예를 들어, 표준 출력을 파일로 보내면서 오류는 화면에 표시되도록 할 수 있습니다.

보다 고급 로깅 및 오류 처리를 위해, `spdlog` 또는 `boost.log` 같은 제3자 라이브러리를 사용할 수 있습니다. 이들 라이브러리는 로깅을 위한 형식 지정, 로그 레벨, 파일 출력을 포함한 향상된 기능을 제공합니다.

오류 메시지를 작성하기 위해 `spdlog`를 사용하는 방법은 여기에 있습니다:

```cpp
#include "spdlog/spdlog.h"

int main() {
    // spdlog 초기화
    spdlog::info("이것은 일반 메시지입니다.");
    spdlog::error("이것은 오류 메시지입니다.");
    
    return 0;
}
```

참고: `spdlog`를 사용하기 위해서는 프로젝트에 추가해야 합니다. GitHub에서 저장소를 클론하거나 `vcpkg` 또는 `conan`과 같은 패키지 매니저를 사용하여 할 수 있습니다.

표준 스트림을 직접 사용하거나 `spdlog` 같은 라이브러리를 사용하는 선택은 어플리케이션의 복잡성과 오류 처리 및 로깅에 대한 특정 요구 사항에 따라 달라집니다.
