---
aliases:
- /ko/powershell/capitalizing-a-string/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:07.195832-07:00
description: "PowerShell\uC5D0\uC11C \uBB38\uC790\uC5F4\uC744 \uB300\uBB38\uC790\uB85C\
  \ \uB9CC\uB4DC\uB294 \uAC83\uC740 \uC8FC\uC5B4\uC9C4 \uBB38\uC790\uC5F4\uC758 \uCCAB\
  \ \uAE00\uC790\uB97C \uB300\uBB38\uC790\uB85C \uBCC0\uD658\uD558\uBA74\uC11C \uB098\
  \uBA38\uC9C0 \uBB38\uC790\uC5F4\uC740 \uBCC0\uACBD\uD558\uC9C0 \uC54A\uB294 \uC791\
  \uC5C5\uC744 \uB9D0\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740\
  \ \uC885\uC885 \uC0AC\uC6A9\uC790 \uC778\uD130\uD398\uC774\uC2A4\uC5D0\uC11C \uD14D\
  \uC2A4\uD2B8\uB97C \uD45C\uC2DC\uD558\uAC70\uB098 \uC0DD\uC131\uB41C \uBB38\uC11C\
  \uC5D0\uC11C \uBB38\uBC95 \uADDC\uCE59\uC744 \uB530\uB974\uAE30 \uC704\uD574 \uC774\
  \uB7EC\uD55C \uC791\uC5C5\uC744 \uC218\uD589\uD569\uB2C8\uB2E4."
lastmod: 2024-02-18 23:09:06.525270
model: gpt-4-0125-preview
summary: "PowerShell\uC5D0\uC11C \uBB38\uC790\uC5F4\uC744 \uB300\uBB38\uC790\uB85C\
  \ \uB9CC\uB4DC\uB294 \uAC83\uC740 \uC8FC\uC5B4\uC9C4 \uBB38\uC790\uC5F4\uC758 \uCCAB\
  \ \uAE00\uC790\uB97C \uB300\uBB38\uC790\uB85C \uBCC0\uD658\uD558\uBA74\uC11C \uB098\
  \uBA38\uC9C0 \uBB38\uC790\uC5F4\uC740 \uBCC0\uACBD\uD558\uC9C0 \uC54A\uB294 \uC791\
  \uC5C5\uC744 \uB9D0\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740\
  \ \uC885\uC885 \uC0AC\uC6A9\uC790 \uC778\uD130\uD398\uC774\uC2A4\uC5D0\uC11C \uD14D\
  \uC2A4\uD2B8\uB97C \uD45C\uC2DC\uD558\uAC70\uB098 \uC0DD\uC131\uB41C \uBB38\uC11C\
  \uC5D0\uC11C \uBB38\uBC95 \uADDC\uCE59\uC744 \uB530\uB974\uAE30 \uC704\uD574 \uC774\
  \uB7EC\uD55C \uC791\uC5C5\uC744 \uC218\uD589\uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uB300\uBB38\uC790\uD654"
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
