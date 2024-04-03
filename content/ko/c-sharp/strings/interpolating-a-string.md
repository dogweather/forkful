---
changelog:
- 2024-02-25, gpt-4-0125-preview, translated from English
date: 2024-02-25 17:07:12.380490-07:00
description: "C#\uC5D0\uC11C \uBB38\uC790\uC5F4 \uBCF4\uAC04\uC740 \uBB38\uC790\uC5F4\
  \ \uB9AC\uD130\uB7F4 \uC548\uC5D0 \uD45C\uD604\uC2DD\uC744 \uD3EC\uD568\uD558\uC5EC\
  \ \uC0C8 \uBB38\uC790\uC5F4\uC744 \uC0DD\uC131\uD558\uAC8C \uD574\uC8FC\uC5B4 \uBB38\
  \uC790\uC5F4\uC758 \uD615\uC2DD\uC744 \uC9C0\uC815\uD558\uACE0 \uC5F0\uACB0\uD558\
  \uAE30 \uC27D\uAC8C \uD574 \uC90D\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\
  \uC740 \uB3D9\uC801 \uBB38\uC790\uC5F4 \uB0B4\uC6A9\uC744 \uB2E4\uB8F0 \uB54C \uCF54\
  \uB4DC\uC758 \uAC00\uB3C5\uC131\uACFC \uC720\uC9C0\uBCF4\uC218\uC131\uC744 \uAC1C\
  \uC120\uD558\uAE30 \uC704\uD574 \uC774 \uAE30\uB2A5\uC744 \uC0AC\uC6A9\uD569\uB2C8\
  \uB2E4."
lastmod: '2024-03-13T22:44:55.213729-06:00'
model: gpt-4-0125-preview
summary: "C#\uC5D0\uC11C \uBB38\uC790\uC5F4 \uBCF4\uAC04\uC740 \uBB38\uC790\uC5F4\
  \ \uB9AC\uD130\uB7F4 \uC548\uC5D0 \uD45C\uD604\uC2DD\uC744 \uD3EC\uD568\uD558\uC5EC\
  \ \uC0C8 \uBB38\uC790\uC5F4\uC744 \uC0DD\uC131\uD558\uAC8C \uD574\uC8FC\uC5B4 \uBB38\
  \uC790\uC5F4\uC758 \uD615\uC2DD\uC744 \uC9C0\uC815\uD558\uACE0 \uC5F0\uACB0\uD558\
  \uAE30 \uC27D\uAC8C \uD574 \uC90D\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uBCF4\uAC04\uD558\uAE30"
weight: 8
---

## 어떻게 사용하는가:
C#에서 문자열 보간은 달러 기호(`$`)와 문자열 리터럴로 표시됩니다. 변수 이름이나 표현식은 중괄호(`{}`) 안에 포함됩니다.

```csharp
string name = "Jane";
int age = 28;
string interpolatedString = $"안녕, {name}! 너는 {age}살이야.";
Console.WriteLine(interpolatedString);
// 출력: 안녕, Jane! 너는 28살이야.
```

더 복잡한 예에서, 중괄호 내에서 연산을 수행하거나 메서드를 호출할 수 있습니다:

```csharp
double price = 19.99;
int quantity = 3;
string orderDetail = $"총 가격: {price * quantity:C2}";
Console.WriteLine(orderDetail);
// 출력: 총 가격: $59.97
```
중괄호 안의 `:C2` 형식 지정자는 숫자를 소수점 두 자리의 통화로 형식을 지정합니다.

더 고급 형식 지정이나 지역화가 필요한 시나리오에서는 `string.Format` 메서드나 Humanizer와 같은 라이브러리를 고려할 수 있습니다. Humanizer는 문자열, 날짜, 시간, 시간 범위, 숫자 및 수량을 더 읽기 쉬운 형식으로 조작하고 표시할 수 있습니다. 아래는 Humanizer를 사용한 복잡한 문자열 조작의 예입니다. Humanizer는 .NET 표준 라이브러리의 일부가 아니며 NuGet 패키지 `Humanizer`를 설치해야 합니다.

먼저, NuGet을 통해 Humanizer를 설치하세요:

```
Install-Package Humanizer
```

그런 다음, 다음과 같이 사용할 수 있습니다:

```csharp
using Humanizer;

int dayDifference = 5;
string humanized = $"이벤트가 {dayDifference}일 전이었습니다.".Humanize();
Console.WriteLine(humanized);
// 구성 및 문화에 따라 가능한 출력: 이벤트가 5일 전이었습니다.
```

이 예제는 기본 사용법을 보여줍니다. Humanizer는 문자열, 날짜, 숫자 등에 적용할 수 있는 다양한 기능을 지원하여 애플리케이션을 더욱 접근하기 쉽고 직관적으로 만듭니다.
