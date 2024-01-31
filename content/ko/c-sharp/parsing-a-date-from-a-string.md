---
title:                "문자열에서 날짜 파싱하기"
date:                  2024-01-20T15:35:40.272691-07:00
simple_title:         "문자열에서 날짜 파싱하기"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜?)
문자열에서 날짜 파싱은 텍스트 형식의 날짜 정보를 날짜와 시간 객체로 변환하는 것입니다. 이 과정은 사용자 입력이나 파일과 같은 외부 소스에서 날짜 데이터를 읽고 이해할 때 필수적입니다.

## How to: (방법)
```C#
using System;
using System.Globalization;

class Program
{
    static void Main()
    {
        // 문자열에서 날짜 파싱
        string dateString = "2023-04-15";
        DateTime parsedDate = DateTime.ParseExact(dateString, "yyyy-MM-dd", CultureInfo.InvariantCulture);
        
        // 결과 출력
        Console.WriteLine(parsedDate); // 출력: 2023-04-15 00:00:00
    }
}
```
날짜 문자열을 `DateTime` 객체로 변환했습니다. `ParseExact` 메서드를 사용해 날짜 포맷을 명시적으로 지정했어요.

## Deep Dive (심층 분석)
날짜 파싱은 .NET의 초기부터 함께 했습니다. 연-월-일 형식은 표준적입니다만, `CultureInfo`를 이용해 다양한 지역과 포맷을 지원합니다. `DateTime.Parse`, `DateTime.ParseExact`, `DateTime.TryParse`, `DateTime.TryParseExact` 같은 메서드들이 있지요.

- `DateTime.Parse`: 날짜와 시간의 문자열 표현을 `DateTime` 객체로 변환합니다.
- `DateTime.TryParse`: 변환에 실패해도 예외를 발생시키지 않고 성공 여부를 반환합니다.
- `DateTime.ParseExact`와 `DateTime.TryParseExact`: 정확한 형식이 알려져 있을 때 사용합니다.

알맞은 메서드를 선택하는 건 중요합니다. `TryParse`와 `TryParseExact`는 변환 실패 시 예외 대신 false를 반환하기 때문에 더 안전한 파싱을 가능하게 합니다. 또한, `CultureInfo.InvariantCulture`를 사용함으로써 문화권-중립적 날짜 포맷을 정의할 수 있습니다.

## See Also (참고 자료)
- [DateTime.Parse Method | Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.parse?view=net-7.0)
- [Custom Date and Time Format Strings | Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)
- [CultureInfo Class | Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo?view=net-7.0)
