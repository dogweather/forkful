---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:53.367889-07:00
description: "C#\uC5D0\uC11C \uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC\uB97C \uD30C\
  \uC2F1\uD558\uB294 \uAC83\uC740 \uB0A0\uC9DC\uC640 \uC2DC\uAC04\uC758 \uD14D\uC2A4\
  \uD2B8 \uD45C\uD604\uC744 `DateTime` \uAC1D\uCCB4\uB85C \uBCC0\uD658\uD558\uB294\
  \ \uAC83\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4. \uC774\uB294 \uB2E4\uC591\uD55C \uD615\
  \uC2DD\uC73C\uB85C \uB0A0\uC9DC\uC640 \uC2DC\uAC04\uC744 \uC870\uC791, \uC800\uC7A5\
  \ \uB610\uB294 \uD45C\uC2DC\uD574\uC57C \uD558\uB294 \uC560\uD50C\uB9AC\uCF00\uC774\
  \uC158\uC5D0 \uD544\uC218\uC801\uC785\uB2C8\uB2E4. \uC608\uB97C \uB4E4\uC5B4 \uC2A4\
  \uCF00\uC904\uB9C1 \uC571, \uB85C\uADF8 \uD504\uB85C\uC138\uC11C, \uC0AC\uC6A9\uC790\
  \ \uB610\uB294\u2026"
lastmod: '2024-03-11T00:14:29.168040-06:00'
model: gpt-4-0125-preview
summary: "C#\uC5D0\uC11C \uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC\uB97C \uD30C\uC2F1\
  \uD558\uB294 \uAC83\uC740 \uB0A0\uC9DC\uC640 \uC2DC\uAC04\uC758 \uD14D\uC2A4\uD2B8\
  \ \uD45C\uD604\uC744 `DateTime` \uAC1D\uCCB4\uB85C \uBCC0\uD658\uD558\uB294 \uAC83\
  \uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4. \uC774\uB294 \uB2E4\uC591\uD55C \uD615\uC2DD\
  \uC73C\uB85C \uB0A0\uC9DC\uC640 \uC2DC\uAC04\uC744 \uC870\uC791, \uC800\uC7A5 \uB610\
  \uB294 \uD45C\uC2DC\uD574\uC57C \uD558\uB294 \uC560\uD50C\uB9AC\uCF00\uC774\uC158\
  \uC5D0 \uD544\uC218\uC801\uC785\uB2C8\uB2E4. \uC608\uB97C \uB4E4\uC5B4 \uC2A4\uCF00\
  \uC904\uB9C1 \uC571, \uB85C\uADF8 \uD504\uB85C\uC138\uC11C, \uC0AC\uC6A9\uC790 \uB610\
  \uB294\u2026"
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC \uBD84\uC11D\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
C#에서 문자열에서 날짜를 파싱하는 것은 날짜와 시간의 텍스트 표현을 `DateTime` 객체로 변환하는 것을 포함합니다. 이는 다양한 형식으로 날짜와 시간을 조작, 저장 또는 표시해야 하는 애플리케이션에 필수적입니다. 예를 들어 스케줄링 앱, 로그 프로세서, 사용자 또는 외부 소스로부터 날짜 입력을 처리하는 모든 시스템 등이 있습니다.

## 방법:

**기본 파싱:**

문자열을 `DateTime`으로 변환하기 위한 주요 선택지는 `DateTime.Parse` 및 `DateTime.TryParse` 메서드입니다. 다음은 간단한 예시입니다:

```csharp
string dateString = "2023-04-12";
DateTime parsedDate;

if (DateTime.TryParse(dateString, out parsedDate))
{
    Console.WriteLine($"성공적으로 파싱됨: {parsedDate}");
}
else
{
    Console.WriteLine("파싱 실패.");
}
// 출력: 성공적으로 파싱됨: 2023년 4월 12일 오전 12:00:00
```

**문화권 지정:**

특정 문화권 형식의 날짜 문자열을 파싱해야 할 때가 있습니다. 이를 위해 `CultureInfo` 클래스를 사용할 수 있습니다:

```csharp
using System.Globalization;

string dateString = "12 avril 2023";
var cultureInfo = new CultureInfo("fr-FR");
DateTime parsedDate = DateTime.Parse(dateString, cultureInfo);

Console.WriteLine(parsedDate);
// 출력: 2023년 4월 12일 오전 12:00:00
```

**특정 형식으로 정확한 파싱:**

표준이 아닐 수 있는 특정 형식으로 날짜가 제공되는 시나리오의 경우, `DateTime.ParseExact`가 유용합니다:

```csharp
string dateString = "Wednesday, 12 April 2023";
string format = "dddd, d MMMM yyyy";
DateTime parsedDate = DateTime.ParseExact(dateString, format, CultureInfo.InvariantCulture);

Console.WriteLine(parsedDate);
// 출력: 2023년 4월 12일 오전 12:00:00
```

**NodaTime 사용하기:**

더 강력한 날짜 및 시간 파싱을 위해, 인기 있는 타사 라이브러리인 NodaTime을 고려해 보세요. NodaTime은 더 넓은 범위의 날짜/시간 처리 기능을 제공합니다:

```csharp
using NodaTime;
using NodaTime.Text;

var pattern = LocalDatePattern.CreateWithInvariantCulture("yyyy-MM-dd");
var parseResult = pattern.Parse("2023-04-12");

if (parseResult.Success)
{
    LocalDate localDate = parseResult.Value;
    Console.WriteLine(localDate); // 2023-04-12
}
else
{
    Console.WriteLine("파싱 실패.");
}
```

NodaTime은 시간대, 기간 및 지속성 개념, 다양한 달력 시스템에 대한 광범위한 지원을 제공하며, .NET 애플리케이션에서 복잡한 날짜 및 시간 조작을 위한 강력한 선택입니다.
