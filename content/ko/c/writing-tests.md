---
title:                "C: 테스트 작성하기"
programming_language: "C"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/writing-tests.md"
---

{{< edit_this_page >}}

## 왜

초보 프로그래머라면 테스트 작성을 무시하고 코드를 쉽게 작성할 수 있습니다. 하지만, 더 나은 코딩 습관을 가지려면 테스트 작성이 중요합니다. 테스트 작성을 통해 코드를 확실하게 이해하고 버그를 찾는데 시간을 절약할 수 있습니다.

## 방법

테스트 작성 방법은 간단합니다. 우선 코드를 작성하고 ``assert``를 사용해 단언문(assertion)을 작성하면서 코드의 예상 결과와 실제 결과가 일치하는지 확인합니다. 코드에서 문제가 발생했을 때 테스트가 실패하기 때문에 어떤 부분에서 문제가 발생했는지 쉽게 파악할 수 있습니다.

```
#include <stdio.h>
#include <assert.h>

int add(int a, int b) {
    return a + b;
}

int main() {
    // 정상 동작하는 경우
    assert(add(2, 3) == 5);
    
    // 문제가 있는 경우
    assert(add(5, 5) == 10);
    return 0;
}
```

위 코드에서 두 번째 ``assert``는 실패하게 됩니다. 결과적으로 테스트를 통해 버그를 찾을 수 있게 됩니다.

물론 위의 예시는 간단한 함수를 사용하여 설명한 것이기 때문에 실제 프로젝트에서는 더 복잡한 테스트 코드를 작성해야 할 수도 있습니다. 테스트를 작성할 때는 프로젝트의 요구사항을 잘 이해하고 해당 코드가 어떤 동작을 하는지 정확히 파악해야 합니다.

## 깊게 파보기

테스트 작성은 프로그래밍에서 불가피한 요소입니다. 하지만 테스트를 작성하는 것만으로 문제가 해결되는 것은 아닙니다. 예를 들어, 테스트 작성에 너무 많은 시간을 투자하게 된다면 실제 코드 작성에 소비하는 시간이 줄어들게 됩니다. 따라서, 효율적으로 테스트를 작성하고 테스트를 통해 버그를 찾는 방법을 연습하는 것이 중요합니다.

## 참고 자료

- [C 프로그래밍 입문자를 위한 assert 구문 소개](https://ko.wikipedia.org/wiki/%EB%B8%94%EB%A3%A8%ED%8C%85_%EB%A8%B8%EC%8B%A0)
- [C 언어를 사용한 단위 테스트 작성 방법](http://www.macs.hw.ac.uk/~hwloidl/CBook/chap2.pdf)