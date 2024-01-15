---
title:                "테스트 쓰기"
html_title:           "C: 테스트 쓰기"
simple_title:         "테스트 쓰기"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/writing-tests.md"
---

{{< edit_this_page >}}

## 왜 테스트를 작성하는가?

프로그래머들은 코드를 작성하는 것만으로는 충분하지 않습니다. 우리는 코드를 실제로 테스트하고 동작을 확인해야만 합니다. 그래야만 우리의 코드가 버그없이 신뢰할 수 있고 안정적인 프로그램을 만들 수 있습니다.

## 어떻게 테스트를 작성할 수 있나요?

테스트를 작성하는 것은 매우 간단합니다. 우선, 우리는 우리가 테스트하고 싶은 함수나 코드 블록을 선택합니다. 그리고 그것을 확인하기 위한 테스트 케이스를 만듭니다. 이제, 우리는 다음과 같이 코드를 작성합니다:

```C
#include <stdio.h>

int multiply(int a, int b) {
  return a * b;
}

void test_multiply() {
  int result = multiply(5, 10);
  if(result == 50) {
    printf("테스트를 통과했습니다!");
  } else {
    printf("테스트를 실패했습니다ㅠㅠ");
  }
}

int main() {
  test_multiply();
  return 0;
}
```

위 코드를 실행하면 "테스트를 통과했습니다!"라는 메시지를 볼 수 있습니다. 이제 우리는 코드를 수정하고 다양한 케이스를 추가하여 더 많은 테스트를 작성할 수 있습니다.

## 더 깊게 들어가보기

테스트를 작성하는 것은 프로그램을 작성하는 것과 같은 중요한 과정입니다. 이를 통해 우리는 코드를 더 효율적으로 작성할 수 있고, 버그를 잡아낼 수 있으며, 나중에 코드를 수정할 때도 안정적인 프로그램을 유지할 수 있습니다. 또한 테스트를 작성하는 것은 개발 과정에서 실수를 방지할 수 있는 유용한 방법이기도 합니다.

## 관련 링크

- [C 전체 프로그래밍 가이드](https://ko.wikipedia.org/wiki/C_언어)
- [반복적인 테스트의 중요성](https://www.ted.com/talks/kent_beck_continuously_testing_software)
- [TDD(Test-Driven Development)에 대한 소개](https://medium.com/@mauriciogior22/%EC%84%A0%ED%98%95%EC%9D%98-tdd-test-driven-development-a13088b71af8)