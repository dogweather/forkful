---
title:    "C: 표준 오류에 쓰는 것"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# 왜 C 프로그래밍에서 표준 에러에 쓰는 걸까요?

프로그램을 작성할 때 표준 출력과 표준 에러를 구분하는 것은 중요합니다. 표준 출력은 사용자에게 결과를 보여주는 역할을 하지만, 표준 에러는 오류 메시지를 표시하는 역할을 합니다. 이를 통해 사용자는 발생한 오류를 확인하고 문제를 해결할 수 있습니다. 따라서 C 프로그래밍에서 표준 에러에 대한 이해는 중요합니다.

## 어떻게 표준 에러에 쓸 수 있을까요?

아래의 예제 코드를 통해 표준 에러에 쓰는 방법을 알아보겠습니다.

```C
#include <stdio.h>
#include <stdlib.h>

int main() {
    int div = 0;
    int num = 10;
    int result = num / div;
    if (div == 0) {
        fprintf(stderr, "오류: 0으로 나눌 수 없습니다\n");
        exit(1);
    }
    fprintf(stdout, "결과: %d\n", result);
    return 0;
}
```

위의 코드에서는 0으로 나누는 경우를 처리하는 부분에서 `fprintf` 함수를 사용하여 표준 에러에 오류 메시지를 출력하고 프로그램을 종료합니다. 이를 통해 사용자는 오류가 발생한 이유를 쉽게 파악할 수 있습니다.

출력 결과는 다음과 같습니다.

```
오류: 0으로 나눌 수 없습니다
```

위의 예제는 단순한 예시이지만, 실제 프로그램에서도 표준 에러를 적절히 활용하여 오류를 처리하는 것이 중요합니다.

## 깊게 이해해보기

C 프로그래밍에서 표준 에러를 쓰는 방법은 매우 다양합니다. 위의 예제에서 사용한 `fprintf` 함수를 대신하여 `fputs` 함수를 사용할 수도 있습니다. 또는 `fprintf` 함수를 사용할 때에는 `stderr` 파일 포인터를 사용하는 것이 좋습니다.

또한 표준 에러의 출력은 사용자에게 보여지지 않으므로, 로그 파일 등을 이용하여 프로그램의 오류를 기록하는 것도 중요합니다. 이를 통해 오류가 발생한 이유를 추적하고 해결할 수 있습니다.

# 더 알아보기

위의 예제 코드에서도 사용한 `fprintf` 함수와 `stderr` 파일 포인터에 대해 더 자세히 알고 싶다면 아래의 링크를 참고해보세요.

* [fprintf 함수 설명 (C언어 레퍼런스)](http://www.cplusplus.com/reference/cstdio/fprintf/)
* [stderr 파일 포인터 설명 (C언어 레퍼런스)](http://www.cplusplus.com/reference/cstdio/stderr/)
* [표준 에러 관리 (Geeksforgeeks)](https://www.geeksforgeeks.org/error-handling-c-programs/)

# 관련 링크

* [표준 출력과 표준 에러의 차이점 (C 언어 레퍼런스)](http://www.cplusplus.com/reference/cstdio/stderr/)
* [fprintf와 stdout의 차이점 (Geeksforgeeks)](https://www.geeksforgeeks.org/difference-printf-sprintf-c/)
* [표준 에러 관리에 대한 더 자세한 설명 (TutorialsPoint)](https://www.tutorialspoint.com/cprogramming/c_error_handling.htm)