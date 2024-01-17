---
title:                "테스트 작성하기"
html_title:           "C: 테스트 작성하기"
simple_title:         "테스트 작성하기"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/writing-tests.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

프로그래머들은 자신들이 만든 코드가 잘 작동하는지 확신하기 위해 테스트를 작성합니다. 테스트란 간단한 입력값을 주고, 그에 따른 출력값을 검증하는 것입니다. 이를 통해 버그를 발견하고 해결할 수 있습니다.

## 하는 법:

```C
// 예시 프로그램
#include <stdio.h>

// 함수 정의
int add(int a, int b) {
  return a + b;
}

// 테스트 케이스
int main() {
  // 입력값을 3과 5로 지정하여 함수를 불러옴
  int result = add(3, 5);
  
  // 정답과 결과값이 일치하는지 확인
  if(result == 8) {
    printf("테스트 성공!");
  } else {
    printf("테스트 실패ㅠㅠ");
  }
  return 0;
}

// 출력: 테스트 성공!
```

## 깊이 알아보기:

테스트는 소프트웨어 개발의 일부로 발전해왔습니다. 예전에는 직접 코드를 실행하며 버그를 찾아서 고치는 시간이 많이 걸렸지만, 지금은 자동화된 테스트 프레임워크를 사용하여 효율적으로 버그를 검출할 수 있습니다. 대표적인 프레임워크로는 JUnit이 있습니다.

## 관련 자료:

- https://ko.wikipedia.org/wiki/테스트_기법
- https://woowabros.github.io/experience/2020/04/01/morm-test-isolation.html