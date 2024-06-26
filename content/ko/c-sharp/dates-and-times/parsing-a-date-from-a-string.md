---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:53.367889-07:00
description: "\uBC29\uBC95: **\uAE30\uBCF8 \uD30C\uC2F1:** \uBB38\uC790\uC5F4\uC744\
  \ `DateTime`\uC73C\uB85C \uBCC0\uD658\uD558\uAE30 \uC704\uD55C \uC8FC\uC694 \uC120\
  \uD0DD\uC9C0\uB294 `DateTime.Parse` \uBC0F `DateTime.TryParse` \uBA54\uC11C\uB4DC\
  \uC785\uB2C8\uB2E4. \uB2E4\uC74C\uC740 \uAC04\uB2E8\uD55C \uC608\uC2DC\uC785\uB2C8\
  \uB2E4."
lastmod: '2024-04-05T21:53:56.971912-06:00'
model: gpt-4-0125-preview
summary: "**\uAE30\uBCF8 \uD30C\uC2F1:** \uBB38\uC790\uC5F4\uC744 `DateTime`\uC73C\
  \uB85C \uBCC0\uD658\uD558\uAE30 \uC704\uD55C \uC8FC\uC694 \uC120\uD0DD\uC9C0\uB294\
  \ `DateTime.Parse` \uBC0F `DateTime.TryParse` \uBA54\uC11C\uB4DC\uC785\uB2C8\uB2E4\
  ."
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC \uBD84\uC11D\uD558\uAE30"
weight: 30
---

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
