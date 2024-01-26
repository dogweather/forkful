---
title:                "디버거 사용하기"
date:                  2024-01-26T04:10:44.166312-07:00
model:                 gpt-4-0125-preview
simple_title:         "디버거 사용하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/using-a-debugger.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
디버거 사용은 실행 중인 코드를 테스트하고 검사하기 위해 특수 도구를 활용하는 것을 의미합니다. 코드의 내부 작동 과정을 확인하고, 버그를 찾아내며, 코드의 동작을 더 잘 이해할 수 있게 해주기 때문에 매우 중요합니다.

## 사용 방법:
Xcode(스위프트용 IDE)에서 디버거를 사용하려면, 중단점을 설정하고, 변수를 검사하며, 표현식을 관찰할 수 있습니다. 예시는 다음과 같습니다:

```Swift
func findFactorial(of number: Int) -> Int {
    if number == 0 {
        return 1
    }
    return number * findFactorial(of: number - 1)
}

let result = findFactorial(of: 5)
print(result)
```

Xcode에서 줄 번호 왼쪽을 클릭하여 중단점을 설정하고, 프로그램을 실행하세요. 중단점에 도달하면, Xcode는 실행을 일시 중지합니다. 이제 다음을 할 수 있습니다:

1. 변수 값을 확인합니다.
2. 디버거 컨트롤을 사용하여 다음 줄을 실행(Step over)하거나 함수 내부로 들어갑니다(Step into).
3. 특정 변수나 상수의 변경을 모니터링하기 위해 '감시 목록'에 표현식을 추가합니다.

디버그 영역에서 볼 수 있는 예시입니다:

```
(lldb) po number
5
(lldb) po result
120
```

## 심층 탐구:
디버거는 1940년대 이래 프로그래밍 환경의 일부였으며, 단순한 중단점 시스템에서 복잡한 UI 기반 경험까지 발전했습니다. Xcode의 내장 디버거 이외에도, Xcode가 내부적으로 사용하는 LLDB(저수준 디버거)와 같은 타사 도구도 있습니다. 어떤 이들은 `print()` 문을 사용하여 디버깅하는 경우도 있으나(애정을 담아 "원시적 디버깅"이라고도 함), 이는 대규모 프로젝트나 복잡한 버그에 대해서는 덜 효율적입니다. 디버거를 사용할 때, 실행 제어, 런타임 내부 조사, 데이터 조작을 다루고 있습니다. 이 원칙들에 대한 깊은 이해는 효율적인 디버깅에 큰 도움이 됩니다.

## 참고 자료:
- [애플의 Xcode 디버깅 가이드](https://developer.apple.com/documentation/xcode/debugging-your-app)
- [LLDB 빠른 시작 가이드](https://lldb.llvm.org/use/tutorial.html)
- [레이 웬더리히의 스위프트 디버깅 튜토리얼](https://www.raywenderlich.com/966538-arc-and-memory-management-in-swift)