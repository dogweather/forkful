---
title:                "C: 테스트 작성하기"
simple_title:         "테스트 작성하기"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/writing-tests.md"
---

{{< edit_this_page >}}

## 왜?

코딩을 할 때 간혹 우리는 코드가 작동할 지 확신하지 못합니다. 그래서 테스트 코드를 작성하는 것은 매우 중요합니다. 테스트 코드를 작성하면 우리는 우리의 코드가 예상대로 작동하는지 확인할 수 있고, 버그를 잡을 수 있으며, 코드를 수정하고 새로운 기능을 추가하는 작업에서 확신을 가지게 됩니다.

## 어떻게 하나요?

테스트 코드를 작성하는 방법은 매우 간단합니다. 우선 코드 블록을 사용하여 기능을 정의합니다. 그리고 그 기능이 예상대로 작동하는지 확인하기 위해 간단한 입력과 해당하는 출력을 정의합니다.

```C
#include <stdio.h>

// 기능 정의
int add(int x, int y) {
    return x + y;
}

int main() {
    // 입력과 출력 정의
    int input = 5;
    int output = add(input, 3);

    // 출력
    printf("%d + 3 = %d", input, output);
    return 0;
}
```

출력: `5 + 3 = 8`

위의 코드는 `add()` 함수를 정의하고, `add()` 함수에 `input` 변수와 `3`을 입력으로 넣어서 `output` 변수에 저장하고, `output` 변수를 출력하는 간단한 예시입니다. 코드를 실행해보면 `output` 변수에는 `8`이 저장되어 출력된 것을 확인할 수 있습니다.

이처럼 간단한 코드 블록을 작성하면서 기능을 정의하고 입력과 출력을 정의하고 실행해보면서 코드가 예상대로 작동하는지 확인할 수 있습니다.

## 깊이 들어가기

테스트 코드를 작성함으로써 우리는 코드의 신뢰성을 높이고, 버그를 잡을 수 있습니다. 테스트 코드를 작성할 때 가장 중요한 것은 가능한 모든 시나리오를 고려하는 것입니다. 예를 들어, `add()` 함수에 음수 입력을 넣었을 때도 예상대로 작동하는지 테스트해야 합니다.

또한, 테스트 코드는 새로운 기능을 추가할 때 매우 유용합니다. 새로운 기능을 추가할 때 기존 코드가 영향을 받지 않도록 기능의 입출력을 정확히 작성하고 테스트 코드를 실행해보면 새로운 기능이 기존 코드와 충돌하지 않고 제대로 작동하는지 확인할 수 있습니다.

## 참고 자료

- [C 언어 테스트 코드 작성 방법](https://www.tutorialspoint.com/cprogramming/c_testing.htm)
- [프로젝트 단위 테스트와 C 언어](http://blog.naver.com/PostView.nhn?blogId=seungho0326&amp;logNo=130019568480)
- [더 나은 코드를 위한 몇 가지 C 언어 테스트 팁](http://blog.naver.com/sgHong1/221087354413)