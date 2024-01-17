---
title:                "디버그 출력 프린팅"
html_title:           "C: 디버그 출력 프린팅"
simple_title:         "디버그 출력 프린팅"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

디버그 출력을 출력하는 것은 프로그래머들이 사용하는 디버깅 기술입니다. 이는 코드 실행 중에 중간 결과를 확인하고 디버그 목적으로 사용됩니다.

## 방법:

디버그 출력을 사용하기 위해서는 ```printf()``` 함수를 사용하여 출력할 내용을 지정해주어야 합니다. 예시 코드는 다음과 같습니다.

```C
#include <stdio.h>
int main() {
    int num = 5;
    printf("현재 num의 값은 %d입니다.\n", num);
}
```

이 코드를 실행하면 디버그 창에 `현재 num의 값은 5입니다.` 라는 결과가 출력됩니다.

## 깊이 들어가보기:

디버그 출력은 현재 버전의 C언어에서도 사용되는 중요한 디버깅 기술입니다. 이는 예전부터 프로그래머들이 사용해오던 방법으로, 코드를 실행하면서 중간 결과를 확인하기 위해 사용됩니다.

디버그 출력 외에도 C언어에서는 `assert`문을 사용하여 디버깅할 수도 있으며, `gcc` 컴파일러에서는 `-DDEBUG` 옵션을 사용하여 디버그를 할 수도 있습니다. 또한 디버그 출력은 버그를 찾는 데 유용하지만, 성능 저하를 야기할 수 있으므로 프로그래머는 적절히 사용해야 합니다.

## 참고:

- [C 프로그래밍 언어 - 위키백과, 우리 모두의 백과사전](https://ko.wikipedia.org/wiki/C_%ED%94%84%EB%A1%9C%EA%B7%B8%EB%9E%A8%EB%93%9C)
- [C 프로그래밍의 역사 - 위키백과, 우리 모두의 백과사전](https://ko.wikipedia.org/wiki/C_%ED%94%84%EB%A1%9C%EA%B7%B8%EB%9E%A8%EB%93%9C%EC%9D%98_%EC%97%AD%EC%82%AC)
- [The Evolution of Debugging: From Gordian Knots to the JDI - Oracle](https://www.oracle.com/technetwork/server-storage/developer-tools/jdi-evolution-087348.html)