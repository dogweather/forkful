---
date: 2024-01-20 17:36:32.976999-07:00
description: "How to: (\uBC29\uBC95) \uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\
  \uD658\uD558\uB294 \uAC83\uC740 .NET Framework \uCD08\uAE30 \uBC84\uC804\uBD80\uD130\
  \ \uC9C0\uC6D0\uB418\uC5C8\uC2B5\uB2C8\uB2E4. ToString \uBA54\uC18C\uB4DC\uB294\
  \ \uB2E4\uC591\uD55C \uC624\uBC84\uB85C\uB4DC\uB97C \uAC00\uC9C0\uACE0 \uC788\uC5B4\
  , DateTime \uAC1D\uCCB4\uB97C \uB2E4\uC591\uD55C \uCEEC\uCC98\uC640 \uD3EC\uB9F7\
  \uC5D0 \uC801\uD569\uD55C \uBB38\uC790\uC5F4\uB85C \uB9CC\uB4E4 \uC218 \uC788\uB3C4\
  \uB85D \uD569\uB2C8\uB2E4. ToString \uBA54\uC18C\uB4DC\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.974443-06:00'
model: gpt-4-1106-preview
summary: "(\uBC29\uBC95) \uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\uD558\
  \uB294 \uAC83\uC740 .NET Framework \uCD08\uAE30 \uBC84\uC804\uBD80\uD130 \uC9C0\uC6D0\
  \uB418\uC5C8\uC2B5\uB2C8\uB2E4."
title: "\uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\uD558\uAE30"
weight: 28
---

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
