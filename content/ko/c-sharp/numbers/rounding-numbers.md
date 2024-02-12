---
title:                "숫자 반올림하기"
aliases:
- /ko/c-sharp/rounding-numbers/
date:                  2024-01-26T03:43:30.516858-07:00
model:                 gpt-4-0125-preview
simple_title:         "숫자 반올림하기"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/rounding-numbers.md"
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
