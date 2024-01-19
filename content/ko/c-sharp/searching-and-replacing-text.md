---
title:                "텍스트 검색 및 교체"
html_title:           "Elixir: 텍스트 검색 및 교체"
simple_title:         "텍스트 검색 및 교체"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 무엇과 왜?

문자열 검색 및 교체는 프로그램 내에서 특정 문자열을 찾고 새 문자열로 교체하는 작업을 말합니다. 개발자들은 이를 사용하여 데이터를 다루거나 사용자에게 필요한 정보를 제공합니다.

## 어떻게 하는가:

사용 가능한 C# 내장 메소드를 사용해 문자열 검색과 교체를 수행하는 방법을 살펴봅시다.

```C#
string originalText = "Hello, World!";
string searchText = "World";
string replaceText = "Korea";

// String.Replace 메소드를 사용하여 텍스트 교체
string resultText = originalText.Replace(searchText, replaceText);

Console.WriteLine(resultText);
```

위 코드를 실행하면 `"Hello, Korea!"`가 출력됩니다.

## 깊게 파보기:

방금 제시한 방법은 문자열을 처리하는 가장 일반적인 방법 중 하나입니다. C#은 `.NET Framework` 출시 이래로 이 기능을 제공하고 있었습니다.

C#이 이 도구를 전혀 제공하지 않는다면 개발자들은 문자열을 배열로 변환하여 일치하는 문자/문자열을 찾아서 교체해야 합니다. 하지만 이러한 접근 방식은 시간 소모적이고 비효율적입니다.

대안으로 정규식(Regex)을 사용하여 고급 문자열 검색 및 교체 작업도 가능합니다. 이러한 접근법은 복잡한 교체 패턴이 필요한 경우 매우 유용합니다.

사용한 메소드 string.Replace에는 두개의 매개변수가 필요합니다: 찾을 텍스트와 교체할 텍스트입니다. 이 메소드는 원본 문자열에서 매개변수로 전달된 "찾을 텍스트"를 검색하고 이를 "교체할 텍스트"로 변경한 새 문자열을 반환합니다.

## 참고자료:

다음 링크를 참고하면 더 많은 정보를 얻을 수 있습니다.

- 문자열 교체 메소드에 대한 Microsoft 공식 문서: https://docs.microsoft.com/en-us/dotnet/api/system.string.replace
- C#의 정규 표현식에 대한 자세한 가이드: https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions
- 문자열 검색 및 교체에 대한 StackOverflow 토론: https://stackoverflow.com/questions/6275980/string-replace-vs-stringbuilder-replace