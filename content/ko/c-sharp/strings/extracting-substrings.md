---
date: 2024-01-20 17:45:11.393115-07:00
description: "How to: (\uBC29\uBC95) C#\uC5D0\uC11C \uBB38\uC790\uC5F4 \uCD94\uCD9C\
  \uC740 \uC8FC\uB85C `Substring` \uBA54\uC18C\uB4DC\uB85C \uC218\uD589\uD569\uB2C8\
  \uB2E4. 2000\uB144 .NET \uD504\uB808\uC784\uC6CC\uD06C\uC758 \uCCAB \uB4F1\uC7A5\
  \uBD80\uD130 \uC0AC\uC6A9\uB410\uC5B4\uC694. \uB300\uC548\uC73C\uB85C\uB294 `Span<T>`\
  \ \uD639\uC740 \uC815\uADDC \uD45C\uD604\uC2DD(Regex)\uC774 \uC788\uC2B5\uB2C8\uB2E4\
  . `Substring`\uC740 \uC0C8\uB85C\uC6B4 \uBB38\uC790\uC5F4\uC744 \uBC18\uD658\uD558\
  \uC9C0\uB9CC,\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.949049-06:00'
model: gpt-4-1106-preview
summary: "(\uBC29\uBC95) C#\uC5D0\uC11C \uBB38\uC790\uC5F4 \uCD94\uCD9C\uC740 \uC8FC\
  \uB85C `Substring` \uBA54\uC18C\uB4DC\uB85C \uC218\uD589\uD569\uB2C8\uB2E4."
title: "\uBD80\uBD84 \uBB38\uC790\uC5F4 \uCD94\uCD9C"
weight: 6
---

## How to: (방법)
```C#
using System;

class SubstringExample {
    static void Main() {
        // 예제 문자열
        string example = "안녕하세요, C# 세계에 오신 것을 환영합니다!";

        // Substring을 이용한 추출
        string greet = example.Substring(0, 5); // "안녕하세요"
        string welcome = example.Substring(14, 2); // "환영"

        // 결과 출력
        Console.WriteLine(greet);
        Console.WriteLine(welcome);
    }
}
```

## Deep Dive (심층 학습)
C#에서 문자열 추출은 주로 `Substring` 메소드로 수행합니다. 2000년 .NET 프레임워크의 첫 등장부터 사용됐어요. 대안으로는 `Span<T>` 혹은 정규 표현식(Regex)이 있습니다. `Substring`은 새로운 문자열을 반환하지만, `Span<T>`는 메모리를 절약할 수 있는 방법입니다. 성능이 중요한 상황에서는 `Span<T>` 사용을 고려해보세요.

## See Also (참고 자료)
- Microsoft의 Substring 메소드 문서: [Substring Method in C#](https://learn.microsoft.com/en-us/dotnet/api/system.string.substring?view=netcore-3.1)
- 정규 표현식 (Regex) 사용법에 대한 문서: [Regular Expressions in C#](https://learn.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference)
- `Span<T>`에 대한 정보: [Span<T> in C#](https://learn.microsoft.com/en-us/dotnet/api/system.span-1?view=netcore-3.1)
