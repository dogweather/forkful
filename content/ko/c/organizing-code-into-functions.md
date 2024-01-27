---
title:                "코드를 함수로 구성하기"
date:                  2024-01-26T01:09:47.156166-07:00
model:                 gpt-4-1106-preview
simple_title:         "코드를 함수로 구성하기"
programming_language: "C"
category:             "C"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
코드를 함수로 구성하는 것은 코드를 특정 작업을 수행하는 재사용 가능한 블록으로 나누는 것입니다. 이렇게 하면 코드를 읽고 디버깅하며 유지보수하기가 쉬워집니다.

## 방법:
간단한 예를 들어봅시다: 두 수를 여러 번 더하고 싶다고 가정해 보겠습니다.

함수 없이:
```C
#include <stdio.h>

int main() {
    int sum1 = 5 + 3;
    printf("합계1: %d\n", sum1);
    
    int sum2 = 2 + 8;
    printf("합계2: %d\n", sum2);
    
    // 더 많은 덧셈들...
    
    return 0;
}
```

함수를 사용하여:
```C
#include <stdio.h>

int add(int a, int b) {
    return a + b;
}

int main() {
    int sum1 = add(5, 3);
    printf("합계1: %d\n", sum1);
    
    int sum2 = add(2, 8);
    printf("합계2: %d\n", sum2);
    
    // 더하기 함수 add()를 사용하여 추가 덧셈 수행...
    
    return 0;
}
```

출력 결과:
```
합계1: 8
합계2: 10
```

## 심층 탐구
C에 함수가 없던 시절, 프로그래밍은 종종 레시피처럼 선형적으로 이루어졌습니다. 그러나 프로그램이 커지면서 코드 중복은 문제가 되었습니다. 함수는 해결책이었으며 - 같은 코드 블록을 프로그램의 다른 부분에서 다시 작성하지 않고도 실행할 수 있도록 해주었습니다. 이를 통해 공간뿐만 아니라 업데이트 시 시간도 절약할 수 있습니다: 한 곳에서 함수를 변경하면, 그것을 사용하는 코드의 모든 부분이 업데이트됩니다.

함수의 대안으로는 인라인 코드, 매크로, 혹은 복사-붙여넣기 방식의 코딩 등이 있을 수 있지만, 이들은 코드가 불필요하게 길어지고 오류 발생 가능성이 높으며 유지보수하기 어려운 코드를 초래할 수 있습니다. 함수는 대조적으로 기능을 캡슐화하고, 분명한 인터페이스를 정의하며, 범위를 올바르게 사용하면 부작용을 줄일 수 있습니다.

함수를 구현할 때, 몇 가지 사항을 고려해야 합니다: 첫째, 함수가 단 한 가지 일만 하도록 만드는 것이 좋으며, 이를 단일 책임 원칙이라고 합니다. 둘째, 이름은 중요합니다 - 함수와 그 매개변수에 서술적인 이름을 선택하여 코드가 자체 문서화 되도록 만드십시오.

## 참고 자료
C의 함수에 대해 더 알아보려면 다음을 확인해 보세요:

- C 표준 라이브러리 참조: https://en.cppreference.com/w/c/header
- C 프로그래밍: 현대적 접근 방식, K.N. 킹 저: 함수에 대한 심층적인 설명을 담고 있는 책입니다.
- Learn-C.org: 함수 섹션: https://www.learn-c.org/en/Functions