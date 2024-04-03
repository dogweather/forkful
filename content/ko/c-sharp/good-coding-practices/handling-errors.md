---
date: 2024-01-26 00:50:20.048250-07:00
description: "\uC5B4\uB5BB\uAC8C: try-catch \uBE14\uB85D\uC73C\uB85C \uC2DC\uC791\uD569\
  \uC2DC\uB2E4. \uC774\uAC83\uC740 \uC904\uD0C0\uAE30\uD558\uB294 \uC0AC\uB78C \uC544\
  \uB798\uC5D0 \uC548\uC804\uB9DD\uC744 \uC124\uCE58\uD558\uB294 \uAC83\uACFC \uAC19\
  \uC2B5\uB2C8\uB2E4. \uB9CC\uC57D \uADF8\uB4E4\uC774 \uBBF8\uB044\uB7EC\uC9C4\uB2E4\
  \uBA74, \uCD94\uB77D\uD558\uC9C0 \uC54A\uACE0 \uC7A1\uD799\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.244856-06:00'
model: gpt-4-1106-preview
summary: "try-catch \uBE14\uB85D\uC73C\uB85C \uC2DC\uC791\uD569\uC2DC\uB2E4."
title: "\uC5D0\uB7EC \uCC98\uB9AC\uD558\uAE30"
weight: 16
---

## 어떻게:
try-catch 블록으로 시작합시다. 이것은 줄타기하는 사람 아래에 안전망을 설치하는 것과 같습니다. 만약 그들이 미끄러진다면, 추락하지 않고 잡힙니다.

```C#
using System;

class ErrorHandlingExample {
    static void Main() {
        try {
            int[] numbers = {1, 2, 3};
            Console.WriteLine(numbers[5]);  // 이런, 인덱스가 범위를 벗어났어요!
        } catch (IndexOutOfRangeException e) {
            Console.WriteLine("오류가 잡혔습니다: " + e.Message);
        }
    }
}
```

잘못될 때 샘플 출력:
```
오류가 잡혔습니다: 인덱스가 배열의 범위를 벗어났습니다.
```

이제 finally 블록을 추가합니다—무슨 일이 있어도 일어나는 일들, 마치 세금을 내는 것처럼요.

```C#
try {
    // 여기에 문제가 될 수 있는 코드
} catch (SomeSpecificException e) {
    // 특정 오류를 여기에서 처리하세요
} finally {
    // 이 코드는 상단에서 무슨 일이 일어나든 실행됩니다
    Console.WriteLine("이 코드는 항상 실행됩니다.");
}
```

## 심층 탐구
오류 처리는 C#이 출시된 이래로 있었습니다. 시간이 지남에 따라 발전했습니다. 예전에 프로그래머들은 문제를 신호하기 위해 반환 코드 또는 글로벌 플래그에 의존했었는데, 이는 복잡하고 오류가 발생하기 쉬웠죠.

C#은 예외라는 더 현대적인 접근 방식을 사용합니다. 예기치 않은 일이 발생했을 때 예외가 발생하며, 이는 풋볼에서 플레이에 페널티 플래그를 던지는 것과 같습니다. try, catch, finally 블록으로 구성된 구조화된 예외 처리는 오래된 방식의 오류 검사보다 이러한 순간을 관리하는 것을 더 명확하고 깔끔하게 만듭니다.

대안? 물론입니다. 예외가 빠져나갈 때에는 `UnhandledExceptionEventHandler`를 사용할 수 있습니다. 또는 비동기 코드에서는 예외를 자체적으로 가진 `Task` 객체로 오류 처리가 약간 달리 이루어집니다.

구현 세부 사항—미세한 부분에 해당—은 중요합니다. 예외는 성능을 끌어내리는 비용이 많이 들 수 있으므로, 일상적인 논리 제어가 아닌 예외적인 경우에만 사용해야 합니다.

## 추가 정보
- [C#에서의 예외에 대한 공식 문서](https://docs.microsoft.com/ko-kr/dotnet/csharp/fundamentals/exceptions/exception-handling)
- [C# 예외 처리의 모범 사례](https://docs.microsoft.com/ko-kr/dotnet/standard/exceptions/best-practices-for-exceptions)
