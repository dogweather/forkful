---
date: 2024-01-20 17:47:02.942464-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) `Length` \uC18D\uC131\
  \uC740 .NET Framework \uCD08\uAE30\uBD80\uD130 \uC788\uC5C8\uC2B5\uB2C8\uB2E4. \uBB38\
  \uC790\uC758 \uC218\uB97C \uBC18\uD658\uD558\uB294 \uAC83\uC774\uC9C0\uB9CC, UTF-16\
  \ \uC778\uCF54\uB529 \uB54C\uBB38\uC5D0 \uC774\uBAA8\uD2F0\uCF58\uC774\uB098 \uD2B9\
  \uBCC4\uD55C \uBB38\uC790\uB294 \uB354 \uB9CE\uC740 \uAE38\uC774\uB97C \uAC00\uC9C8\
  \ \uC218 \uC788\uC2B5\uB2C8\uB2E4. `StringInfo` \uD074\uB798\uC2A4\uB098\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:09.562941-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) `Length` \uC18D\uC131\uC740 .NET\
  \ Framework \uCD08\uAE30\uBD80\uD130 \uC788\uC5C8\uC2B5\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC758 \uAE38\uC774 \uCC3E\uAE30"
weight: 7
---

## How to: (어떻게 하나요?)
```C#
string exampleText = "안녕하세요!";
int length = exampleText.Length;

Console.WriteLine(length); // 출력: 6
```

## Deep Dive (심화 학습)
`Length` 속성은 .NET Framework 초기부터 있었습니다. 문자의 수를 반환하는 것이지만, UTF-16 인코딩 때문에 이모티콘이나 특별한 문자는 더 많은 길이를 가질 수 있습니다. `StringInfo` 클래스나 `System.Globalization` 네임스페이스를 통해 더 복잡한 문자 처리를 할 수 있습니다.

## See Also (추가 자료)
- [Microsoft Documentation on String.Length Property](https://docs.microsoft.com/en-us/dotnet/api/system.string.length)
- [.NET Globalization and Localization](https://docs.microsoft.com/en-us/dotnet/standard/globalization-localization/)
- [Understanding Text in .NET](https://docs.microsoft.com/en-us/dotnet/standard/base-types/character-encoding)
