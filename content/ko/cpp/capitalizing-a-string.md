---
title:                "문자열 대문자로 변환하기"
date:                  2024-01-19
html_title:           "Arduino: 문자열 대문자로 변환하기"
simple_title:         "문자열 대문자로 변환하기"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)

문자열 대문자 변환은 각 문자를 대문자로 바꾸는 것입니다. 데이터 정규화, 사용자 인터페이스 향상, 또는 프로그램의 일관성을 위해 사용합니다.

## How to: (방법)

```C++
#include <iostream>
#include <string>
#include <algorithm>

int main() {
    std::string str = "hello world!";
    std::transform(str.begin(), str.end(), str.begin(), 
                   [](unsigned char c){ return std::toupper(c); });

    std::cout << str << std::endl;
    return 0;
}
```

출력:
```
HELLO WORLD!
```

## Deep Dive (심층 분석)

과거 C에서는 `toupper` 함수를 이용해 문자를 대문자로 변환했습니다. C++에서는 `<algorithm>`의 `std::transform`을 사용하여, 람다 함수와 함께 더 간편하고 효율적으로 문자열 전체를 대문자로 변환할 수 있습니다. `std::for_each`와 같은 다른 알고리듬을 사용할 수도 있지만, `std::transform`이 더 적합합니다. 비표준 대안으로는 각 언어 환경에 맞는 문자 변환을 적용하는 라이브러리가 있으나, 표준 라이브러리만으로도 충분히 구현 가능합니다.

## See Also (참고 자료)

- C++ `std::transform` reference: https://en.cppreference.com/w/cpp/algorithm/transform
- C++ `std::toupper` reference: https://en.cppreference.com/w/cpp/string/byte/toupper
- More about Lambda Expressions in C++: https://en.cppreference.com/w/cpp/language/lambda
