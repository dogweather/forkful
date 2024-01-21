---
title:                "디버그 출력을 찍어보기"
date:                  2024-01-20T17:52:11.190625-07:00
model:                 gpt-4-1106-preview
simple_title:         "디버그 출력을 찍어보기"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가요? 왜 그런가요?)
디버그 출력은 프로그램 실행 중 정보를 콘솔에 표시하는 것입니다. 프로그래머는 버그를 찾고 프로그램 이해를 증진하기 위해 이를 사용합니다.

## How to: (어떻게 하나요?)
C++에서 디버그 정보를 출력하는 기본적인 방법입니다. `iostream` 라이브러리를 활용한 예시코드와 출력결과를 보여드립니다.

```C++
#include <iostream>

int main() {
    int sum = 0;
    // Debug Output
    for (int i = 0; i <= 10; ++i) {
        sum += i;
        // Print the current value of i and sum
        std::cout << "i: " << i << ", sum: " << sum << std::endl;
    }
    return 0;
}

/*
Sample Output:
i: 0, sum: 0
i: 1, sum: 1
i: 2, sum: 3
...
i: 10, sum: 55
*/
```

## Deep Dive (자세한 정보)
디버그 출력은 UNIX 시스템에서 터미널을 사용할 때부터 있었습니다. `printf`는 초기에 많이 사용된 함수지만, C++에서는 `iostream` 라이브러리가 표준입니다. 대안으로는 `std::cerr`가 있어 에러와 관련된 디버그 정보를 출력할 때 유용합니다. 또한, 상황에 맞게 출력을 끄거나 켜는 맞춤설정을 할 수도 있습니다(`#ifdef DEBUG`와 같은 매크로 사용). 

성능이 중요한 상황에서는 I/O 연산이 오버헤드를 일으킬 수 있으므로 주의해서 사용해야 합니다. 개발과정에서는 유용하지만, 실제 운영 환경에서는 로그 파일이나 다른 메커니즘을 활용하는 것이 좋습니다.

## See Also (더 보기)
- C++ Reference for `iostream`: https://en.cppreference.com/w/cpp/header/iostream
- Debugging strategies: https://en.cppreference.com/w/cpp/language/debug
- Conditional Compilation: https://en.cppreference.com/w/cpp/preprocessor/conditional