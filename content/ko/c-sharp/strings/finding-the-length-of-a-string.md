---
title:                "문자열의 길이 찾기"
aliases:
- /ko/c-sharp/finding-the-length-of-a-string/
date:                  2024-01-20T17:47:02.942464-07:00
model:                 gpt-4-1106-preview
simple_title:         "문자열의 길이 찾기"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열 길이 찾기는 글자 수를 세는 것입니다. 데이터 유효성 검사, 입력 처리, 루프 제어에서 필수적으로 사용됩니다.

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
