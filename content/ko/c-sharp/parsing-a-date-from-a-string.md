---
title:                "문자열에서 날짜 분석하기"
html_title:           "Gleam: 문자열에서 날짜 분석하기"
simple_title:         "문자열에서 날짜 분석하기"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열에서 날짜를 파싱하는 것은 특정 문자열 형식의 날짜를 DateTime 객체로 변환하는 과정입니다. 이는 프로그래머들이 날짜와 시간 데이터를 용이하게 조작하고 사용하기 위해 필요한 작업입니다.

## 어떻게 하나요:

다음은 C#에서 문자열에서 날짜를 파싱하는 코드입니다:

```C#
string dateString = "2022-07-21";
DateTime parsedDate;

if (DateTime.TryParse(dateString, out parsedDate))
{
    Console.WriteLine("파싱 성공: {0}", parsedDate);
}
else
{
    Console.WriteLine("파싱 실패");
}
```
이 코드는 문자열 "2022-07-21"을 DateTime 객체로 파싱합니다. 'TryParse' 메서드는 파싱을 시도하고, 성공하면 true를 반환하고 'parsedDate'에 변환 된 DateTime 객체를 설정합니다. 그렇지 않으면 false를 반환합니다.

이 코드의 출력 예는 다음과 같습니다:

```
파싱 성공: 2022-07-21 00:00:00
```

## 심화 내용:

1. **역사적 맥락**: C#에서 날짜와 시간을 파싱하고 조작하는 기능은 프로그래밍 언어의 초기 버전부터 존재했습니다. 일관된 작업을 위해 .NET은 국제 표준을 따르는 DateTime과 DateTimeOffset 유형을 제공합니다.

2. **대안**: "DateTime.Parse"와 "DateTime.TryParse" 외에도 "DateTime.ParseExact"와 "DateTime.TryParseExact" 메서드를 사용하여 날짜와 시간 형식의 문자열을 파싱할 수 있습니다. 이러한 메서드는 특정 형식을 명확하게 지정할 수 있다는 장점이 있습니다.

3. **구현 세부사항**: 파싱하기 전 문자열의 형식이 DateTime 형식 (예: "yyyy-MM-dd")과 일치해야합니다. 아닐 경우 파싱에 실패합니다.

## 참고 자료:

- MS Docs의 DateTime 구조에 대한 설명: <https://docs.microsoft.com/ko-kr/dotnet/api/system.datetime?view=net-5.0>
- Date and Time Format in C#: <https://www.c-sharpcorner.com/blogs/date-and-time-format-in-c-sharp-programming1>
- C# - Date & Time: <https://www.tutorialspoint.com/csharp/csharp_date_time.htm>