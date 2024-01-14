---
title:    "Arduino: 테스트 작성하기"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## 왜 테스트를 작성해야 할까요?

아두이노 프로그래밍을 하다보면 코드의 작동 여부를 확인하기 위해 테스트하는 것이 중요합니다. 테스트를 작성할 수 있으면 코드를 신뢰하고, 잠재적인 버그를 발견하고 수정할 수 있습니다. 따라서 효율적인 아두이노 프로그래밍을 위해서는 테스트 작성이 필수적입니다.

## 방법

테스트를 작성하는 방법에 대해 알아보겠습니다. 아래의 코드 블록은 두 수를 더하는 간단한 아두이노 함수 예제입니다.

```Arduino
int add(int a, int b) {
  return a + b;
}
```

테스트 함수는 "assert" 문을 사용하여 함수의 리턴 값과 실제 값을 비교합니다. 아래는 위에서 만든 함수에 대한 테스트 예제입니다. "assert" 문은 주어진 조건이 참이 아닌 경우에만 출력됩니다. 따라서 함수의 리턴 값과 8이 다른 경우에만 출력됩니다.

```Arduino
void test_add() {
  assert(add(3, 5) == 8);
}
```

테스트 함수를 실행하려면 "test_add()"를 호출하면 됩니다. 별도의 코드를 작성하지 않고도 아두이노 IDE의 시리얼 모니터를 통해 테스트 결과를 확인할 수 있습니다. 아래는 위의 예제를 실행한 결과입니다.

```
test_add() passed!
```

더 복잡한 함수를 테스트할 때는 여러 개의 "assert" 문을 사용하면 됩니다. 예를 들어, 아래와 같이 두 수를 곱하는 함수를 테스트하는 경우, 두 개의 "assert" 문을 사용하여 다양한 입력값에 대한 테스트를 진행할 수 있습니다.

```Arduino
int multiply(int a, int b) {
  return a * b;
}

void test_multiply() {
  assert(multiply(2, 3) == 6);
  assert(multiply(-2, 5) == -10);
}
```

## 더 깊게 들어가보기

위의 예제에서는 단순히 예외 상황에 대해서만 테스트를 진행했지만, 좀 더 정교한 테스트를 진행할 수도 있습니다. 예를 들어, 함수가 제대로 작동하는 경우에 대해서도 테스트할 수 있습니다. 아래 예제는 두 수를 더하는 함수의 세 번째 인자로 결과값을 저장하는 경우를 테스트하는 예제입니다.

```Arduino
void test_add() {
  int result = 0;
  assert(add(3, 5, result) == 8);
  assert(result == 8);
}
```

더 많은 테스트 예제와 테스트 방법에 대해서는 아두이노 프로그래밍과 관련된 강의나 문서를 참고하시기 바랍니다.

## 참고 자료

- [아두이노 테스트(Test) 기초](https://ko.wikipedia.org/wiki/아두이노_테스트_(Test) 기초)
- [아두이노를 제대로 쓰려면 테스트를 하자](https://www.sparkfun.com/news/343)
- [아두이노 테스트 코드 작성하기](https://www.arduino.cc/en/Hacking/LibraryTutorial#toc9)