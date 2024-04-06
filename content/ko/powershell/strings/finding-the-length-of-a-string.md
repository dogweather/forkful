---
date: 2024-01-20 17:48:05.133170-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C:) PowerShell\uC5D0\uC11C \uBB38\uC790\uC5F4\
  \uC758 \uAE38\uC774\uB97C \uAD6C\uD558\uB294 \uCF54\uB4DC \uC608\uC81C\uC640 \uACB0\
  \uACFC\uB97C \uBD05\uC2DC\uB2E4."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.190556-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C:) PowerShell\uC5D0\uC11C \uBB38\uC790\uC5F4\uC758 \uAE38\
  \uC774\uB97C \uAD6C\uD558\uB294 \uCF54\uB4DC \uC608\uC81C\uC640 \uACB0\uACFC\uB97C\
  \ \uBD05\uC2DC\uB2E4."
title: "\uBB38\uC790\uC5F4\uC758 \uAE38\uC774 \uCC3E\uAE30"
weight: 7
---

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
