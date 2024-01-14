---
title:                "Python: 디버그 출력 출력하기"
simple_title:         "디버그 출력 출력하기"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## 왜
프로그램을 디버깅할 때, 디버그 출력을 프린트하는 것은 중요합니다. 디버그 출력은 프로그램의 상태를 이해하는 데 도움이되며 오류를 추적하는 데 도움이됩니다.

## 하는 방법
```Python
# 예제 코드
def factorial(n):
    if n == 1:
        return 1
    else:
        return n * factorial(n-1)

debug = True # 디버그 모드 활성화
print("n: ", n) # 변수 n의 값 출력

result = factorial(5) # factorial 함수 실행
print("결과: ", result) # 결과값 출력
```

위의 코드는 재귀함수를 사용한 팩토리얼 함수의 예제입니다. 디버그 모드를 활성화하고 변수의 값을 출력함으로써 어떤 문제를 해결해야하는지 더 쉽게 파악할 수 있습니다. 또한 함수가 제대로 작동하는지 결과값도 함께 확인할 수 있습니다.

출력 예시:

```
n: 5
n: 4
n: 3
n: 2
결과: 120
```

## 깊이 파고들기
디버그 출력은 프로그램을 이해하는 데 있어 매우 유용합니다. 프로그램이 제대로 작동하지 않을 경우, 출력을 통해 어떤 부분에서 오류가 발생했는지 쉽게 파악할 수 있습니다. 또한 출력을 통해 변수의 값과 함수의 실행 순서를 확인할 수 있으며 이를 통해 오류를 추적하고 수정하는 데 도움이됩니다.

디버그 출력을 활용할 때 주의해야 할 점은 디버그 모드를 활성화하고 출력을 프린트한 후 꼭 모드를 비활성화하는 것입니다. 디버그 모드를 활성화한 채로 프로그램을 실행하면 출력이 많아져 성능에 영향을 미칠 수 있습니다. 그리고 디버그 출력을 모드가 활성화되지 않은 상태로 프린트하면 코드가 실행되지 않으므로 주의해야합니다.

## 또 다른 정보
- [파이썬 디버깅 기술](https://programmers.co.kr/learn/courses/9/lessons/195)
- [간단하고 빠른 디버깅 방법](https://www.daleseo.com/python-debugging/)