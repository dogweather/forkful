---
title:                "문자열을 소문자로 변환하기"
aliases:
- ko/powershell/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:39:11.080344-07:00
model:                 gpt-4-1106-preview
simple_title:         "문자열을 소문자로 변환하기"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열을 소문자로 변환한다는 건, 문자열 내의 모든 대문자를 소문자로 바꾸는 것을 말합니다. 프로그래머들은 이 작업을 주로 데이터를 일관되게 처리하거나, 대소문자를 구별하지 않는 검색을 수행할 때 사용합니다.

## How to (어떻게 하나요?)
PowerShell에서 문자열을 소문자로 바꾸는 방법은 간단합니다. `.ToLower()` 메서드를 사용하면 돼요. 아래 예제 코드를 실행해보세요.

```PowerShell
# 변수에 문자열 할당
$str = "HELLO, WORLD!"

# 소문자로 변환
$lowerStr = $str.ToLower()

# 결과 출력
$lowerStr
```

실행 결과는 다음과 같이 나와야 합니다:

```
hello, world!
```

가끔은 전체 파이프라인을 통해 여러 항목에 적용할 수 있습니다:

```PowerShell
# 배열의 모든 문자열을 소문자로 변환
$strings = "FIRST", "SECOND", "THIRD"
$strings | ForEach-Object { $_.ToLower() }
```

결과:

```
first
second
third
```

## Deep Dive (심층 분석)
과거에는 대소문자 변환 작업이 컴퓨터에 많은 자원을 소모했어요. 하지만 현대의 PowerShell은 이 작업을 빠르고 효율적으로 합니다. 대안으로는 `$str.toLowerInvariant()`가 있습니다. 이것은 특정 문화권에 관계없이 변환을 실행합니다. 이해하려면 유니코드와 문화권이 어떻게 문자 변환에 영향을 미치는지 알아야 하죠. 소문자 변환은 유니코드 표준에 기반하여 작동하며, 각 문자는 고유 코드 포인트를 가지고 있어요.

## See Also (관련 링크)
- [String Methods - Microsoft .NET Documentation](https://docs.microsoft.com/en-us/dotnet/api/system.string?view=net-5.0)
