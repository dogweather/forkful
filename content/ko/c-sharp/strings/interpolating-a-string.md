---
aliases:
- /ko/c-sharp/interpolating-a-string/
date: 2024-01-20 17:50:26.150930-07:00
description: "\uBB38\uC790\uC5F4 \uBCF4\uAC04\uC774\uB780, \uBB38\uC790\uC5F4 \uC548\
  \uC5D0 \uBCC0\uC218\uB098 \uD45C\uD604\uC2DD\uC758 \uAC12\uC744 \uC9D1\uC5B4\uB123\
  \uB294 \uAC83\uC785\uB2C8\uB2E4. \uCF54\uB4DC\uB97C \uAE54\uB054\uD558\uAC8C \uC720\
  \uC9C0\uD558\uACE0 \uAC00\uB3C5\uC131\uC744 \uB192\uC774\uAE30 \uC704\uD574 \uC0AC\
  \uC6A9\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: 2024-02-18 23:09:06.197983
model: gpt-4-1106-preview
summary: "\uBB38\uC790\uC5F4 \uBCF4\uAC04\uC774\uB780, \uBB38\uC790\uC5F4 \uC548\uC5D0\
  \ \uBCC0\uC218\uB098 \uD45C\uD604\uC2DD\uC758 \uAC12\uC744 \uC9D1\uC5B4\uB123\uB294\
  \ \uAC83\uC785\uB2C8\uB2E4. \uCF54\uB4DC\uB97C \uAE54\uB054\uD558\uAC8C \uC720\uC9C0\
  \uD558\uACE0 \uAC00\uB3C5\uC131\uC744 \uB192\uC774\uAE30 \uC704\uD574 \uC0AC\uC6A9\
  \uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uBCF4\uAC04\uD558\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇인가요? 왜 사용하나요?)
문자열 보간이란, 문자열 안에 변수나 표현식의 값을 집어넣는 것입니다. 코드를 깔끔하게 유지하고 가독성을 높이기 위해 사용합니다.

## How to: (어떻게 하나요?)
```C#
string name = "세종대왕";
int year = 1443;
string invention = "훈민정음";

// 문자열 보간 사용
string message = $"안녕하세요, {name}입니다. {year}년에 {invention}을 창제했습니다.";
Console.WriteLine(message);
```
출력: 안녕하세요, 세종대왕입니다. 1443년에 훈민정음을 창제했습니다.

## Deep Dive (심층 분석)
C# 6.0부터 문자열 보간이 도입되었고 `$` 기호와 중괄호 `{}`를 사용합니다. 문자열 보간 전에는 `string.Format`을 사용했지만 코드가 복잡해지기 쉽습니다. 문자열 보간은 내부적으로 `string.Format`을 호출하여 처리하지만 훨씬 직관적입니다. 컴파일러는 보간 문자열을 `FormattableString` 객체로 변환하여 컴파일 타임에 문자열을 구성하지 않고, 런타임에 평가합니다.

보간 문자열 내에서는 표현식을 직접 사용할 수도 있습니다:
```C#
int a = 10, b = 20;
string sumMessage = $"10과 20의 합은 {a + b}입니다.";
Console.WriteLine(sumMessage);
```
출력: 10과 20의 합은 30입니다.

## See Also (관련 자료)
- [Microsoft - 문자열 보간($)](https://docs.microsoft.com/ko-kr/dotnet/csharp/language-reference/tokens/interpolated)
- [C# 문자열 포맷](https://docs.microsoft.com/ko-kr/dotnet/api/system.string.format?view=net-6.0)
- [C# 가이드 - 문자열 보간의 사용](https://docs.microsoft.com/ko-kr/dotnet/csharp/language-reference/keywords/interpolated-strings)
