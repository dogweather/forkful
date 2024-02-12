---
title:                "문자열 대문자화"
aliases: - /ko/powershell/capitalizing-a-string.md
date:                  2024-02-03T19:06:07.195832-07:00
model:                 gpt-4-0125-preview
simple_title:         "문자열 대문자화"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇을, 왜?
PowerShell에서 문자열을 대문자로 만드는 것은 주어진 문자열의 첫 글자를 대문자로 변환하면서 나머지 문자열은 변경하지 않는 작업을 말합니다. 프로그래머들은 종종 사용자 인터페이스에서 텍스트를 표시하거나 생성된 문서에서 문법 규칙을 따르기 위해 이러한 작업을 수행합니다.

## 어떻게:
PowerShell은 다재다능한 도구로서, 제3자 라이브러리가 필요 없이 문자열을 대문자로 만드는 간단한 방법을 제공합니다. 다음은 그 방법입니다:

```powershell
# CultureInfo의 내장 .Net 메서드 'ToTitleCase'를 사용
$text = "hello world"
$culture = [System.Globalization.CultureInfo]::InvariantCulture
$capitalizedText = $culture.TextInfo.ToTitleCase($text.ToLower())
Write-Output $capitalizedText
```
출력:
```
Hello world
```

참고: 이 메서드는 각 단어의 첫 글자를 대문자로 만듭니다. 문자열의 첫 글자만 대문자로 만들고 나머지는 그대로 두고 싶다면, 다음과 같이 할 수 있습니다:

```powershell
# 문자열의 첫 문자만 대문자로 만들기
$text = "hello world"
$capitalizedText = $text.Substring(0,1).ToUpper() + $text.Substring(1)
Write-Output $capitalizedText
```
출력:
```
Hello world
```

PowerShell은 문자열의 첫 글자만 대문자로 만드는 간단한 함수를 직접 포함하고 있지 않지만, `Substring(0,1).ToUpper()`와 같은 기본 문자열 조작 메서드와 연결 작업을 결합함으로써 원하는 결과를 쉽게 얻을 수 있습니다.
