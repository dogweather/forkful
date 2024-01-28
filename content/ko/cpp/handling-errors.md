---
title:                "에러 처리하기"
date:                  2024-01-26T00:49:48.211524-07:00
model:                 gpt-4-1106-preview
simple_title:         "에러 처리하기"
programming_language: "C++"
category:             "C++"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/handling-errors.md"
---

{{< edit_this_page >}}

## 오류 처리란 무엇이며 왜 중요한가?
오류 처리란 문제가 발생했을 때를 대비하는 것입니다. 소프트웨어가 충돌을 피하고 견고하며 사용자 친화적이 되도록 도와주기 때문에 매우 중요합니다.

## 어떻게 할까요:
다음은 예외를 처리하기 위한 기본적인 try-catch 블록입니다:

```cpp
#include <iostream>
#include <stdexcept>

int main() {
    try {
        throw std::runtime_error("이런! 무언가 잘못되었습니다.");
    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << std::endl;
    }
    return 0;
}
```

샘플 출력:
```
Error: 이런! 무언가 잘못되었습니다.
```

## 심층 탐구
C++는 초기부터 오류 처리 기능을 가지고 있었습니다. 가장 기본적인 형태는 반환값을 확인하는 것이었습니다. 만약 여러분이 경험 많은 개발자라면, 표준 이전의 시절을 기억할 것입니다: 클래스를 가진 C와 수동 오류 확인.

이후 C++에 예외 처리가 등장하여 예상치 못한 문제들을 처리하기 위한 구조화된 방법을 제공했습니다. 예외는 `throw`로 발생시키고 `try/catch`로 잡습니다.

로직 오류(예를 들어 잘못된 계산)와 런타임 오류(예를 들어 유효하지 않은 메모리 주소에 접근하는 것)와 같은 두 가지 유형의 오류가 종종 발생합니다. 예외는 런타임 오류에 이상적입니다. 로직 오류의 경우에는 주로 어설션(assertions) 또는 오류 코드를 사용하는 것이 더 낫습니다.

예외와 오류 코드에 대한 끊임없는 논쟁이 있습니다. 예외는 더 느릴 수 있으며 복잡한 제어 흐름을 초래할 수 있습니다. 반면 오류 코드는 더 빠르지만 코드를 지저분하게 만들고 유지 보수하기 어렵게 할 수 있습니다. 상황에 따라 절충을 해야 하므로 사용 사례를 아는 것이 중요합니다.

C++17에서는 `std::optional`과 `std::variant`를 도입하여 예외의 대안으로 제시했습니다. 이들은 유효한 결과를 반환할 수도 있고 그렇지 않을 수도 있는 함수에 유용합니다.

예외 안정성 또한 또 다른 고민거리입니다. 예외에도 불구하고 코드가 제공하는 보장에 관한 것입니다. 기본, 강력, nothrow 세 가지 수준이 있습니다. 보장이 많을수록 코드가 복잡해질 수 있습니다.

마지막 생각—오류 처리는 과학만큼이나 예술입니다. 애플리케이션이 야외에서 어떻게 생존하는지를 형성합니다. 예외를 남용하지 마십시오. 가독성 있고 유지 보수 가능한 코드를 목표로 하십시오.

## 참고 자료
- [cppreference의 예외 처리에 대한 설명](https://en.cppreference.com/w/cpp/language/exceptions)
- [Bjarne Stroustrup의 오류 처리에 관한 견해](http://www.stroustrup.com/except.pdf)
- [C++ 코어 가이드라인의 예외에 관한 내용](https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines#Re-exceptions)
