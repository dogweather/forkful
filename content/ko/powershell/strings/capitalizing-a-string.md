---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:07.195832-07:00
description: "\uC5B4\uB5BB\uAC8C: PowerShell\uC740 \uB2E4\uC7AC\uB2E4\uB2A5\uD55C\
  \ \uB3C4\uAD6C\uB85C\uC11C, \uC81C3\uC790 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uAC00 \uD544\
  \uC694 \uC5C6\uC774 \uBB38\uC790\uC5F4\uC744 \uB300\uBB38\uC790\uB85C \uB9CC\uB4DC\
  \uB294 \uAC04\uB2E8\uD55C \uBC29\uBC95\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4. \uB2E4\
  \uC74C\uC740 \uADF8 \uBC29\uBC95\uC785\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.518701-06:00'
model: gpt-4-0125-preview
summary: "PowerShell\uC740 \uB2E4\uC7AC\uB2E4\uB2A5\uD55C \uB3C4\uAD6C\uB85C\uC11C\
  , \uC81C3\uC790 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uAC00 \uD544\uC694 \uC5C6\uC774 \uBB38\
  \uC790\uC5F4\uC744 \uB300\uBB38\uC790\uB85C \uB9CC\uB4DC\uB294 \uAC04\uB2E8\uD55C\
  \ \uBC29\uBC95\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uB300\uBB38\uC790\uD654"
weight: 2
---

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
