---
title:                "문자열을 소문자로 변환하기"
date:                  2024-01-20T17:37:58.196082-07:00
model:                 gpt-4-1106-preview
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
문자열을 소문자로 변환하는 것은 문자열 내의 모든 대문자를 해당하는 소문자로 바꾸는 과정입니다. 프로그래머들은 데이터를 일관되게 처리하거나, 대소문자를 구분하지 않는 비교를 수행하기 위해 이를 사용합니다.

## How to:
C#에서 문자열을 소문자로 바꾸는 것은 쉽습니다. `ToLower()` 함수를 사용하면 됩니다. 예시를 보시죠:

```c#
string originalText = "Hello, World!";
string lowerText = originalText.ToLower();

Console.WriteLine(lowerText); // 출력: hello, world!
```

`ToLowerInvariant()` 함수를 사용하는 것도 좋습니다. 이 함수는 문화권에 상관없이 소문자 변환을 수행합니다.

```c#
string mixedText = "C# 프로그래밍";
string lowerInvariantText = mixedText.ToLowerInvariant();

Console.WriteLine(lowerInvariantText); // 출력: c# 프로그래밍
```

## Deep Dive
C#에서의 소문자 변환은 Unicode 표준을 따릅니다. 문자열을 변환할 때는 해당 로캘의 규칙이 적용되기도 합니다. `ToLower()`는 현 로캘을 고려하고, `ToLowerInvariant()`는 문화권 무관하게 작동합니다.

옛날에는 소문자 변환을 위해 ASCII 코드표를 참고하여 직접 변환하는 경우도 있었습니다. 하지만 이 방법은 국제화된 어플리케이션에는 적합하지 않습니다.

소문자 변환을 위한 대안으로 정규 표현식이나 LINQ 등을 사용할 수도 있지만, 이러한 방법들은 `ToLower()`나 `ToLowerInvariant()`보다 성능과 가독성 면에서 뒤떨어질 수 있습니다.

## See Also
C#에 관한 더 많은 정보를 얻고 싶다면, 다음의 자료를 확인하세요.

- Microsoft의 공식 문서: [ToLower() Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower?view=net-6.0)
- Microsoft의 공식 문서: [ToLowerInvariant() Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolowerinvariant?view=net-6.0)
- [Unicode 표준](https://home.unicode.org/)
