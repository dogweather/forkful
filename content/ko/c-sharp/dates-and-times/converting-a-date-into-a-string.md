---
title:                "날짜를 문자열로 변환하기"
date:                  2024-01-20T17:36:32.976999-07:00
model:                 gpt-4-1106-preview
simple_title:         "날짜를 문자열로 변환하기"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가? 왜 사용하나?)
날짜를 문자열로 변환하는 것은 DateTime 객체를 텍스트 형식으로 바꾸는 과정입니다. 사용자가 이해하기 쉬운 형태로 날짜를 표시하거나 데이터의 형식을 통일하기 위해 프로그래머들은 이 변환을 자주 사용합니다.

## How to: (방법)
```C#
using System;
using System.Globalization;

class DateToStringExample
{
    static void Main()
    {
        DateTime now = DateTime.Now;
        // 기본 ToString() 사용
        string defaultString = now.ToString();
        Console.WriteLine(defaultString); // ex: "2023-04-01 10:15:30"

        // 사용자 정의 포맷 지정
        string customFormat = now.ToString("yyyy년 MM월 dd일 HH시 mm분 ss초");
        Console.WriteLine(customFormat); // ex: "2023년 04월 01일 10시 15분 30초"
        
        // CultureInfo 사용
        string koreanFormat = now.ToString("f", new CultureInfo("ko-KR"));
        Console.WriteLine(koreanFormat); // ex: "2023년 4월 1일 토요일 오전 10:15"
    }
}
```

출력:
```
2023-04-01 10:15:30
2023년 04월 01일 10시 15분 30초
2023년 4월 1일 토요일 오전 10:15
```

## Deep Dive (심화 학습)
날짜를 문자열로 변환하는 것은 .NET Framework 초기 버전부터 지원되었습니다. ToString 메소드는 다양한 오버로드를 가지고 있어, DateTime 객체를 다양한 컬처와 포맷에 적합한 문자열로 만들 수 있도록 합니다. ToString 메소드 외에도, `String.Format()` 또는 문자열 보간(string interpolation)과 같은 방법으로도 날짜를 문자열로 포매팅할 수 있습니다. 예를 들어, `String.Format("오늘은 {0:yyyy년 MM월 dd일}입니다.", DateTime.Now);`나 $`"오늘은 {DateTime.Now:yyyy년 MM월 dd일}입니다.";` 같은 표현이 가능합니다.

날짜 포매팅은 프로그램의 로케일 설정에 크게 의존하므로, 다국어 지원이 필요한 애플리케이션에서는 `CultureInfo` 객체를 사용하여 명시적으로 문화권을 설정하는 것이 중요합니다. `InvariantCulture` 를 사용하면 문화권에 독립적인 포맷을 보장할 수 있습니다.

## See Also (관련 자료)
- [DateTime.ToString 메서드](https://docs.microsoft.com/ko-kr/dotnet/api/system.datetime.tostring)
- [문자열 보간 (C# 참조)](https://docs.microsoft.com/ko-kr/dotnet/csharp/language-reference/tokens/interpolated)
- [CultureInfo 클래스](https://docs.microsoft.com/ko-kr/dotnet/api/system.globalization.cultureinfo)
- [Standard Date and Time Format Strings](https://docs.microsoft.com/ko-kr/dotnet/standard/base-types/standard-date-and-time-format-strings)
- [Custom Date and Time Format Strings](https://docs.microsoft.com/ko-kr/dotnet/standard/base-types/custom-date-and-time-format-strings)
