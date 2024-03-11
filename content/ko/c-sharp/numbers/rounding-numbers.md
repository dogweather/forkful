---
date: 2024-01-26 03:43:30.516858-07:00
description: "\uC22B\uC790\uB97C \uBC18\uC62C\uB9BC\uD55C\uB2E4\uB294 \uAC83\uC740\
  \ \uADF8\uAC83\uB4E4\uC744 \uAC00\uC7A5 \uAC00\uAE4C\uC6B4 \uC9C0\uC815\uB41C \uC790\
  \uB9BF\uC218\uB85C \uC870\uC815\uD558\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\
  \uB2E4\u2014\uB354 \uAC04\uB2E8\uD55C \uD615\uD0DC\uB85C \uAF49 \uC870\uC774\uB294\
  \ \uAC83\uC744 \uC0DD\uAC01\uD574\uBCF4\uC138\uC694. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uC815\uBC00\uB3C4\uB97C \uC81C\uC5B4\uD558\uACE0, \uC131\uB2A5\uC744\
  \ \uD5A5\uC0C1\uC2DC\uD0A4\uAC70\uB098, \uC138 \uAC1C\uC758 \uC18C\uC218\uC810\uC774\
  \ \uD544\uC694 \uC5C6\uB294 \uAC00\uACA9\uACFC \uAC19\uC774 \uC0AC\uC6A9\uC790 \uCE5C\
  \uD654\uC801\uC778 \uACB0\uACFC\uB97C \uBCF4\uC5EC\uC904 \uB54C\u2026"
lastmod: '2024-03-11T00:14:29.145757-06:00'
model: gpt-4-0125-preview
summary: "\uC22B\uC790\uB97C \uBC18\uC62C\uB9BC\uD55C\uB2E4\uB294 \uAC83\uC740 \uADF8\
  \uAC83\uB4E4\uC744 \uAC00\uC7A5 \uAC00\uAE4C\uC6B4 \uC9C0\uC815\uB41C \uC790\uB9BF\
  \uC218\uB85C \uC870\uC815\uD558\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4\
  \u2014\uB354 \uAC04\uB2E8\uD55C \uD615\uD0DC\uB85C \uAF49 \uC870\uC774\uB294 \uAC83\
  \uC744 \uC0DD\uAC01\uD574\uBCF4\uC138\uC694. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\
  \uC740 \uC815\uBC00\uB3C4\uB97C \uC81C\uC5B4\uD558\uACE0, \uC131\uB2A5\uC744 \uD5A5\
  \uC0C1\uC2DC\uD0A4\uAC70\uB098, \uC138 \uAC1C\uC758 \uC18C\uC218\uC810\uC774 \uD544\
  \uC694 \uC5C6\uB294 \uAC00\uACA9\uACFC \uAC19\uC774 \uC0AC\uC6A9\uC790 \uCE5C\uD654\
  \uC801\uC778 \uACB0\uACFC\uB97C \uBCF4\uC5EC\uC904 \uB54C\u2026"
title: "\uC22B\uC790 \uBC18\uC62C\uB9BC\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇이며 왜인가?
숫자를 반올림한다는 것은 그것들을 가장 가까운 지정된 자릿수로 조정하는 것을 의미합니다—더 간단한 형태로 꽉 조이는 것을 생각해보세요. 프로그래머들은 정밀도를 제어하고, 성능을 향상시키거나, 세 개의 소수점이 필요 없는 가격과 같이 사용자 친화적인 결과를 보여줄 때 반올림합니다.

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
