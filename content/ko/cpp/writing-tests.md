---
title:                "C++: 테스트 작성하기"
programming_language: "C++"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/writing-tests.md"
---

{{< edit_this_page >}}

# 왜?

프로그램을 작성할 때, 우리는 모두 정확하고 에러가 없는 코드를 작성하려고 합니다. 그러나 어떤 시점에서 우리는 더 많은 코드를 추가하고 변경하며 코드가 예상대로 작동하는지 확신할 수 없게됩니다. 이 때 테스트를 작성해야합니다. 테스트는 우리의 코드에 대한 확신과 신뢰를 주기 때문입니다.

## 하둡(How To)

테스트를 작성하는 것은 매우 간단합니다. 먼저 우리는 우리의 코드가 제대로 작동하는지 확인할 수 있는 예제를 만들어야합니다. 그런 다음 이 예제를 바탕으로 코드를 작성하고 코드가 예상대로 작동하는지 확인해야합니다. 다음은 어떻게 작성하는지에 대한 예제입니다.

```C++
#include <iostream>
#include <cmath>

int square(int x) {
    return pow(x, 2);
}

int main() {
    int num = 5;
    int result = square(num);
    
    std::cout << "5의 제곱은 " << result << "입니다.";
    return 0;
}
```

위 코드에서 우리는 `square` 함수를 정의하고 정수를 제곱하는 함수를 만들었습니다. 그리고 `main` 함수에서 이 함수를 호출하고 결과를 출력했습니다. 여기서 우리는 `square` 함수가 제대로 제곱하는지 확인할 수 있습니다.

## 심층(Deep Dive)

테스트를 작성하는 또 다른 방법은 단언문(assertion)을 이용하는 것입니다. 단언문은 프로그램이 동작하는 동안 특정 조건이 참인지 검사하는 방법입니다. 아래의 예제를 참조하십시오.

```C++
#include <iostream>
#include <cassert>

int absolute(int x) {
    if (x < 0) {
        x = -x;
    }
    return x;
}

int main() {
    int num = -10;
    int result = absolute(num);
    assert(result == 10);
    
    std::cout << "절대값은 " << result << "입니다.";
    return 0;
}
```

위 코드에서 우리는 `absolute` 함수를 정의하고 주어진 정수의 절대값을 반환합니다. 그리고 `main` 함수에서 이 함수를 호출하고 `assert` 문을 사용해 결과가 올바른지 검사합니다. 이러한 단언문은 테스트 작성에 매우 유용합니다.

## 보기

- [의 자논 디자인: 테스트 작성하기]( https://monsieursurbacco.github.io/blog/2017/10/28/test-driven-design.html)
- [C++의 단언문(assertion) 사용법]( http://egloos.zum.com/nroklee/v/577550)