---
date: 2024-01-20 17:47:02.942464-07:00
description: "\uBB38\uC790\uC5F4 \uAE38\uC774 \uCC3E\uAE30\uB294 \uAE00\uC790 \uC218\
  \uB97C \uC138\uB294 \uAC83\uC785\uB2C8\uB2E4. \uB370\uC774\uD130 \uC720\uD6A8\uC131\
  \ \uAC80\uC0AC, \uC785\uB825 \uCC98\uB9AC, \uB8E8\uD504 \uC81C\uC5B4\uC5D0\uC11C\
  \ \uD544\uC218\uC801\uC73C\uB85C \uC0AC\uC6A9\uB429\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.220616-06:00'
model: gpt-4-1106-preview
summary: "\uBB38\uC790\uC5F4 \uAE38\uC774 \uCC3E\uAE30\uB294 \uAE00\uC790 \uC218\uB97C\
  \ \uC138\uB294 \uAC83\uC785\uB2C8\uB2E4."
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
