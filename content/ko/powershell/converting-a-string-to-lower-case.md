---
title:                "문자열을 소문자로 변환하기"
html_title:           "PowerShell: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 무엇인가요? 왜 사용하나요?

스트링을 소문자로 변환하는 것은 일반적으로 프로그래머들이 하는 작업입니다. 소문자로 변환하면 일관된 문자열 형식을 유지할 수 있고, 비교나 검색 작업에서 유용하게 사용할 수 있기 때문입니다.

## 어떻게 하나요?

```PowerShell
# 문자열을 변수에 할당합니다.
$fruit = "Apple"

# 소문자로 변환하여 출력합니다.
PowerShell $fruit.ToLower()
```

```PowerShell
# 문자열을 변수에 할당합니다.
$name = "John Doe"

# 소문자로 변환하여 출력합니다.
PowerShell $name.ToLower()
```

**출력:**
```PowerShell
apple
john doe
```
## 깊게 들어가보기

### 역사적 맥락
소문자로 변환하기 전에는 일반적으로 문자열의 첫 글자만 대문자로 만들었지만, 최근에는 소문자 형태가 더 많이 사용되고 있습니다. 이는 개발자들이 더 많은 문자열 데이터를 다루는 데 있어서 표준화와 일관성을 유지하기 위함입니다.

### 대안
PowerShell에서는 `ToLower()` 메서드 뿐만 아니라 `.toLower()`와 같은 다양한 메서드들을 제공합니다. 또한 정규표현식을 사용하여 문자열 데이터를 변환할 수도 있습니다.

### 구현 세부사항
PowerShell의 `ToLower()` 메서드는 영어 문자에 대해서만 적용됩니다. 만약 다른 언어의 문자를 변환하고 싶다면, 해당 언어에 맞는 대문자와 소문자의 매핑 정보를 제공해야 합니다.

## 관련 자료

- [Microsoft Docs: "ToLower" 메서드](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower?view=net-5.0)
- [Devblogs: Using ToLower() for String Formatting](https://devblogs.microsoft.com/powershell/using-tolower-when-formatting-strings/)