---
title:                "테스트 작성하기"
html_title:           "Arduino: 테스트 작성하기"
simple_title:         "테스트 작성하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/writing-tests.md"
---

{{< edit_this_page >}}

﻿# 왜 테스트를 작성하는가?

아두이노 프로그램을 작성할 때, 우리는 종종 버그나 오류가 발생하지 않도록 조심해야 합니다. 그래서 우리는 고품질의 코드를 작성하기 위해 테스트를 작성하는 것이 중요합니다. 이러한 테스트는 우리의 코드가 원활하게 작동하며 예상대로 작동하는지를 확인하는 데 도움이 됩니다.

## 작성하는 방법

```Arduino
#include <unity.h> // unity testing library

// 예시 코드 - 두 수를 더하는 함수
int add(int x, int y){
  return x + y;
}

// 테스트 함수 - add 함수가 제대로 작동하는지 확인
void test_add(){
  int result = add(4, 5);
  TEST_ASSERT_EQUAL(9, result); // 예상 결과와 동일한지 확인
}

void setup(){
  UNITY_BEGIN(); // test 시작
  RUN_TEST(test_add); // test 실행
  UNITY_END(); // test 종료
}

void loop(){
  // 테스트 중이 아니므로 아무런 코드가 필요하지 않음
}
```

출력 결과:

```
test_add
✔ check result

-----------------------
1 Tests  | 1 Failures
```

예시 코드에서는 두 수를 더하는 함수를 테스트하였습니다. add 함수가 다른 수를 더하는지 혹은 예상치와 다른 결과를 내는지를 확인하는 것이 중요합니다. 향후 코드가 업데이트되거나 수정되더라도, 이러한 테스트를 통해 코드의 일관성을 유지할 수 있습니다.

## 심층 분석

테스트를 작성할 때에는 작성하는 대상 코드의 모든 케이스를 고려해야 합니다. 예를 들어, 조건문이나 반복문 등을 적절히 테스트하는 것이 중요합니다. 또한, 테스트가 반복적으로 실행될 때마다 같은 결과가 나오는지도 확인해야 합니다. 이를 통해 우리의 코드의 신뢰성을 높일 수 있습니다.

# 또 다른 정보들

- [아두이노 테스팅 기초](https://www.maxembedded.com/2014/01/unit-testing-arduino-sketches/)
- [유니티 라이브러리 공식 문서](https://github.com/ThrowTheSwitch/Unity/blob/master/docs/UnityAssertions.md)
- [코드 품질 향상을 위한 테스트 작성 방법](https://blog.testdouble.com/posts/2019-11-04-how-to-test-during-development/)