---
title:                "문자열 대문자로 변환하기"
date:                  2024-01-19
html_title:           "Arduino: 문자열 대문자로 변환하기"
simple_title:         "문자열 대문자로 변환하기"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열을 대문자로 만드는 것은 각 단어의 첫 글자나 모든 글자를 대문자로 바꾸는 것입니다. 프로그래머들은 보통 이렇게 처리하여 사용자 인터페이스를 깔끔하게 보이게 하거나 데이터의 일관성을 유지하기 위해 사용합니다.

## How to: (어떻게:)
### 단어의 첫 글자만 대문자로
```PowerShell
$exampleString = "안녕하세요, powershell을 배우고 있습니다."
$capitalizedString = $exampleString | ForEach-Object { $_.Substring(0,1).ToUpper() + $_.Substring(1).ToLower() }
$capitalizedString
```
출력:
```
안녕하세요, Powershell을 배우고 있습니다.
```

### 모든 글자를 대문자로
```PowerShell
$exampleString = "안녕하세요, powershell이 재미있습니다."
$uppercasedString = $exampleString.ToUpper()
$uppercasedString
```
출력:
```
안녕하세요, POWERSHELL이 재미있습니다.
```

## Deep Dive (심층 분석)
문자열을 대문자로 바꾸는 것은 컴퓨터프로그래밍 이래로 흔히 사용되어 왔습니다. 데이터베이스 검색에서 대소문자를 구별하지 않게 하거나 텍스트를 보기 좋게 하기 위해서 사용됐죠. PowerShell에서는 `.ToUpper()`와 `.ToLower()` 메소드를 사용하여 쉽게 문자열의 대소문자 변환을 수행할 수 있습니다. 또한, `.ToTitleCase()` 함수를 사용해서 TextInfo 객체를 통해 첫 글자만 대문자로 바꿀 수도 있습니다. 구현의 세부 사항에 있어서, 이 메소드들은 내부적으로 유니코드 표준을 따르고 있어서 다국어에도 잘 대응합니다.

## See Also (참고자료)
- [[.NET의 TextInfo Class에 관한 문서]](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.textinfo.totitlecase?view=net-5.0)
- [유니코드 표준에 대한 공식 문서](https://unicode.org/standard/standard.html)
