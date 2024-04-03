---
date: 2024-01-20 17:48:05.133170-07:00
description: "\uBB38\uC790\uC5F4\uC758 \uAE38\uC774\uB97C \uCC3E\uB294 \uAC83\uC740\
  \ \uBB38\uC790\uC758 \uC218\uB97C \uC138\uB294 \uACFC\uC815\uC774\uB2E4. \uD504\uB85C\
  \uADF8\uB798\uBA38\uB4E4\uC740 \uB370\uC774\uD130 \uAC80\uC99D, \uC785\uB825 \uCC98\
  \uB9AC, UI \uB514\uC790\uC778 \uB610\uB294 \uC131\uB2A5 \uCD5C\uC801\uD654 \uB4F1\
  \ \uC5EC\uB7EC \uBAA9\uC801\uC73C\uB85C \uBB38\uC790\uC5F4 \uAE38\uC774\uB97C \uC54C\
  \uC544\uB0B4\uC57C \uD55C\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.530579-06:00'
model: gpt-4-1106-preview
summary: "\uBB38\uC790\uC5F4\uC758 \uAE38\uC774\uB97C \uCC3E\uB294 \uAC83\uC740 \uBB38\
  \uC790\uC758 \uC218\uB97C \uC138\uB294 \uACFC\uC815\uC774\uB2E4."
title: "\uBB38\uC790\uC5F4\uC758 \uAE38\uC774 \uCC3E\uAE30"
weight: 7
---

## What & Why? (무엇과 왜?)
문자열의 길이를 찾는 것은 문자의 수를 세는 과정이다. 프로그래머들은 데이터 검증, 입력 처리, UI 디자인 또는 성능 최적화 등 여러 목적으로 문자열 길이를 알아내야 한다.

## How to: (어떻게:)
PowerShell에서 문자열의 길이를 구하는 코드 예제와 결과를 봅시다.

```PowerShell
# 변수에 문자열 할당
$string = "안녕하세요, PowerShell!"

# 문자열 길이 구하기
$length = $string.Length

# 결과 출력
Write-Host "문자열의 길이는:" $length
```

출력:
```
문자열의 길이는: 18
```

## Deep Dive (심층 분석)
문자열의 길이를 찾는 작업은 프로그래밍의 초기부터 필요했다. 과거 언어들은 종종 null-terminated 문자열을 사용했고, 길이를 수동으로 계산해야 할 때도 많았다.

현대의 PowerShell은 .NET Framework를 기반으로 하므로 문자열 객체의 `Length` 속성을 사용하여 바로 길이를 구할 수 있다. 그러나 멀티바이트 문자셋을 다루는 경우, `Length` 속성은 실제 출력 문자 수와 다를 수 있다(예: 유니코드 이모지)

대안적으로, `StringInfo` 클래스의 `LengthInTextElements` 속성을 사용하여 텍스트 요소의 실제 수를 구할 수도 있다.

```PowerShell
$string = "👩‍👩‍👧‍👦"

# StringInfo를 사용하여 텍스트의 길이를 얻음
$length = [System.Globalization.StringInfo]::new($string).LengthInTextElements

# 결과 출력
Write-Host "실제 텍스트 요소의 길이는:" $length
```

출력:
```
실제 텍스트 요소의 길이는: 1
```
이 예제에서는 정교한 가족 이모지 문자가 하나의 텍스트 요소로 간주되었다.

## See Also (더 보기)
- .NET의 StringInfo 클래스: [StringInfo Class](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.stringinfo?view=net-6.0)
