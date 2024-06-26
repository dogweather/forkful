---
date: 2024-01-26 03:43:30.516858-07:00
description: "\uBC29\uBC95: \uC61B\uB0A0\uC5D0\uB294 \uC5F0\uC0B0 \uBE44\uC6A9\uC744\
  \ \uC904\uC774\uAE30 \uC704\uD574 \uBC18\uC62C\uB9BC\uC774 \uB2F9\uC5F0\uD55C \uC77C\
  \uC774\uC5C8\uC2B5\uB2C8\uB2E4. \uBAA8\uB4E0 \uC0AC\uC774\uD074\uC774 \uC911\uC694\
  \uD588\uACE0, \uC22B\uC790\uB97C \uC904\uC784\uC73C\uB85C\uC368 \uADC0\uC911\uD55C\
  \ \uC2DC\uAC04\uC744 \uC808\uC57D\uD588\uC2B5\uB2C8\uB2E4. \uD604\uB300 C#\uC73C\
  \uB85C \uAC00\uC18D\uD558\uBA74, \uC774\uB294 double\uACFC decimal\uC758 \uC815\uBC00\
  \uB3C4 \uC624\uB958\uC640 \uD45C\uC2DC \uAE30\uC774\uD568\uC5D0 \uB300\uD55C \uAD00\
  \uB9AC\uC5D0 \uAD00\uD55C \uAC83\uC785\uB2C8\uB2E4.\u2026"
lastmod: '2024-04-05T22:51:09.567268-06:00'
model: gpt-4-0125-preview
summary: "\uC61B\uB0A0\uC5D0\uB294 \uC5F0\uC0B0 \uBE44\uC6A9\uC744 \uC904\uC774\uAE30\
  \ \uC704\uD574 \uBC18\uC62C\uB9BC\uC774 \uB2F9\uC5F0\uD55C \uC77C\uC774\uC5C8\uC2B5\
  \uB2C8\uB2E4."
title: "\uC22B\uC790 \uBC18\uC62C\uB9BC\uD558\uAE30"
weight: 13
---

## 방법:
C#에서 숫자를 반올림하는 왕복 티켓은 다음과 같습니다:

```csharp
using System;

public class RoundingExamples
{
    public static void Main()
    {
        double originalNumber = 123.4567;

        // 가장 가까운 정수로 반올림
        double rounded = Math.Round(originalNumber);
        Console.WriteLine(rounded); // 출력: 123

        // 소수점 자리 지정
        double roundedTwoDecimalPlaces = Math.Round(originalNumber, 2);
        Console.WriteLine(roundedTwoDecimalPlaces); // 출력: 123.46

        // 다음 자릿수와 상관없이 올림
        double roundedUp = Math.Ceiling(originalNumber);
        Console.WriteLine(roundedUp); // 출력: 124

        // 다음 자릿수와 상관없이 내림
        double roundedDown = Math.Floor(originalNumber);
        Console.WriteLine(roundedDown); // 출력: 123
    }
}
```

## 심층 분석
옛날에는 연산 비용을 줄이기 위해 반올림이 당연한 일이었습니다. 모든 사이클이 중요했고, 숫자를 줄임으로써 귀중한 시간을 절약했습니다. 현대 C#으로 가속하면, 이는 double과 decimal의 정밀도 오류와 표시 기이함에 대한 관리에 관한 것입니다.

`Math.Round`, `Math.Floor`, 그리고 `Math.Ceiling`를 넘어서, `MidpointRounding` 열거형은 우리에게 가엾은 중간에 위치한 숫자들의 운명을 결정하게 합니다—그것은 은행 규칙과 "반올림"의 놀이터 공정성 사이의 교차점입니다.

더 까다로운 분야, 예를 들어 심각한 수학이나 금융 애플리케이션의 경우, 우리는 더 높은 정밀도를 제공함으로써 반올림 드라마를 줄이는 `decimal`을 `double`보다 선택합니다—반올림이 적을수록 문제가 적습니다.

## 참고
- [공식 C# 문서의 `Math.Round`](https://docs.microsoft.com/en-us/dotnet/api/system.math.round)
- [Stack Overflow: Decimal 대신 Double을 사용해야 할 때는 언제인가?](https://stackoverflow.com/questions/1165761/decimal-vs-double-which-one-should-i-use-and-when)
- [부동 소수점 산술을 위한 IEEE 표준 (IEEE 754)](https://en.wikipedia.org/wiki/IEEE_754)
