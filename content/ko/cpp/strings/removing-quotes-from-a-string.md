---
date: 2024-01-26 03:38:11.261847-07:00
description: "\uBC29\uBC95: \uB2E4\uC74C\uC740 C++\uC5D0\uC11C \uB530\uC634\uD45C\uB97C\
  \ \uC81C\uAC70\uD558\uB294 \uAC04\uB2E8\uD55C \uBC29\uBC95\uC785\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.651283-06:00'
model: gpt-4-0125-preview
summary: "\uB2E4\uC74C\uC740 C++\uC5D0\uC11C \uB530\uC634\uD45C\uB97C \uC81C\uAC70\
  \uD558\uB294 \uAC04\uB2E8\uD55C \uBC29\uBC95\uC785\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C \uC81C\uAC70\uD558\uAE30"
weight: 9
---

## 방법:
다음은 C++에서 따옴표를 제거하는 간단한 방법입니다:

```cpp
#include <iostream>
#include <algorithm>

std::string remove_quotes(std::string input) {
    input.erase(std::remove(input.begin(), input.end(), '\"'), input.end());
    input.erase(std::remove(input.begin(), input.end(), '\''), input.end());
    return input;
}

int main() {
    std::string original = R"("Hello, 'World'!")";
    std::string no_quotes = remove_quotes(original);
    std::cout << no_quotes << std::endl;
    return 0;
}
```

이것을 실행하면 다음을 얻게 됩니다:

```
Hello, World!
```

보십시오! 따옴표가 사라졌습니다.

## 심층 분석
따옴표는 컴퓨팅의 새벽부터 텍스트의 불편함이었습니다. 옛날에는 프로그래머들이 따옴표를 필터링하기 위해 각 문자를 수고스럽게 순환하는 것을 볼 수 있었습니다. 오늘날에는 표준 템플릿 라이브러리(STL)의 `std::remove`가 중량급 작업을 수행합니다.

대안이 있습니까? 물론입니다! `std::regex`와 정규 표현식을 사용하여 따옴표를 대상으로 할 수 있지만, 이는 간단한 작업에는 과하게 강력한 해결책일 수 있는 - 너무 과한 것일 수 있다는 것입니다. 최근 C++ 버전을 선호하는 이들은 수정하지 않는 접근 방식에 `std::string_view`를 사용할 수도 있습니다.

구현 측면에서 기억할 것은 `std::remove`가 실제로 컨테이너에서 요소를 제거하는 것이 아니라, 제거되지 않은 요소를 앞으로 이동시키고 범위의 새 끝을 넘어서는 반복자를 반환한다는 점입니다. 이것이 우리가 원하지 않는 꼬리를 잘라내기 위해 `erase` 메소드가 필요한 이유입니다.

## 참조
- C++ `std::remove` 참조: [cppreference.com](https://en.cppreference.com/w/cpp/algorithm/remove)
- `std::string` 조작에 대한 자세한 내용: [cplusplus.com](http://www.cplusplus.com/reference/string/string/)
