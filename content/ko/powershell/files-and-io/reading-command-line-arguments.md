---
date: 2024-01-20 17:57:05.986863-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) PowerShell\uC5D0\uC11C\
  \ \uBA85\uB839\uC904 \uC778\uC790\uB97C \uC77D\uC73C\uB824\uBA74 `$args`\uB098 `param`\uC744\
  \ \uC0AC\uC6A9\uD569\uB2C8\uB2E4. `$args`\uB294 \uBAA8\uB4E0 \uC778\uC790\uB97C\
  \ \uBC30\uC5F4\uB85C \uC800\uC7A5\uD558\uACE0, `param`\uC740 \uC2A4\uD06C\uB9BD\uD2B8\
  \ \uC2DC\uC791\uBD80\uC5D0 \uC120\uC5B8\uD574 \uAC01 \uC778\uC790\uB97C \uBCC0\uC218\
  \uB85C \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.569355-06:00'
model: gpt-4-1106-preview
summary: "PowerShell\uC5D0\uC11C \uBA85\uB839\uC904 \uC778\uC790\uB97C \uC77D\uC73C\
  \uB824\uBA74 `$args`\uB098 `param`\uC744 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uBA85\uB839\uC904 \uC778\uC218 \uC77D\uAE30"
weight: 23
---

## How to: (어떻게 하나요?)
PowerShell에서 명령줄 인자를 읽으려면 `$args`나 `param`을 사용합니다. `$args`는 모든 인자를 배열로 저장하고, `param`은 스크립트 시작부에 선언해 각 인자를 변수로 사용합니다.

```PowerShell
# $args 예제
Write-Host "인자들: $args"
```
사용 예시:
```shell
PS> .\script.ps1 인자1 인지2 인자3
인자들: 인자1 인지2 인자3
```

`param` 사용법은 다음과 같습니다:

```PowerShell
# param 예제
param (
    [string]$이름,
    [int]$나이
)
Write-Host "안녕하세요, $이름 님, 나이는 $나이 살입니다."
```
사용 예시:
```shell
PS> .\script.ps1 -이름 홍길동 -나이 26
안녕하세요, 홍길동 님, 나이는 26살입니다.
```

## Deep Dive (심도 있는 정보)
명령줄 인자는 UNIX 시스템부터 시작해, 사용자가 프로그램에 입력을 제공하는 일반적인 방법이 되었습니다. PowerShell은 `$args`와 `param`을 이용해 이전의 CMD나 Bash 스크립트에 비해 더 진보된 인자 처리를 제공합니다. 예를 들어, `param` 블럭 안에서 인자 유형을 지정해 타입 안전성을 확보할 수 있습니다. 또한, `Get-Command -Syntax`나 `Get-Help`와 같은 PowerShell Cmdlet을 사용해 스크립트에 대한 인자 정보를 얻을 수 있습니다.
