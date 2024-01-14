---
title:    "C++: 테스트 작성하기"
keywords: ["C++"]
---

{{< edit_this_page >}}

## 왜 테스트를 작성하는가?
코딩을 할 때 우리는 자동화된 테스트를 만들 필요가 있습니다. 이렇게하면 우리가 작업하는 소프트웨어의 신뢰성을 확실히 보장 할 수 있으며 문제를 최대한 일찍 찾을 수 있기 때문입니다.

## 어떻게 작성하는가?
아래는 C++로 작성된 간단한 코드의 예입니다. "hello"라는 단어를 출력하는 간단한 함수를 테스트하는 것입니다.

```C++
#include <iostream>
#include "hello.h"

int main() {

  if (hello() == "hello") {
    std::cout << "Success!" << std::endl;
  }
  else {
    std::cout << "Failed!" << std::endl;
  }
  
  return 0;
}
```

### 샘플 출력:
```
Success!
```

## 깊게 파고들기
자동화된 테스트를 작성하기 전에, 우리는 무엇을 테스트해야하는지 결정해야합니다. 이를 위해서는 처음부터 잘 설계된 코드가 필요합니다. 또한, 모든 가능한 시나리오를 이해하고 이들을 테스트해야합니다. 여러분이 예기치 않은 버그를 찾을 수 있도록 테스트 범위를 최대한 넓게 설정하는 것이 중요합니다.

## 또 보기
- [Automated Testing in C++] (https://www.youtube.com/watch?v=Vl4xzL2pTj4)
- [Testing Strategies for Beginners] (https://www.geeksforgeeks.org/software-testing-strategies-beginners/)