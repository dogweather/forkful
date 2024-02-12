---
title:                "부분 문자열 추출"
aliases:
- /ko/c-sharp/extracting-substrings/
date:                  2024-01-20T17:45:11.393115-07:00
model:                 gpt-4-1106-preview
simple_title:         "부분 문자열 추출"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (무엇 & 왜?)
문자열에서 특정 부분을 끄집어내는 것을 '부분 문자열 추출'이라고 합니다. 데이터 처리, 입력 검증, 혹은 문자열 형태소를 분석할 때 필요합니다.

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
