---
aliases:
- /ko/powershell/reading-command-line-arguments/
date: 2024-01-20 17:57:05.986863-07:00
description: "\uBA85\uB839\uC904 \uC778\uC790(command line arguments)\uB97C \uC77D\
  \uB294 \uAC83\uC740 \uC2A4\uD06C\uB9BD\uD2B8\uC5D0 \uC785\uB825\uC744 \uC804\uB2EC\
  \uD558\uB294 \uBC29\uBC95\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB294\
  \ \uC774\uB97C \uD65C\uC6A9\uD574 \uC720\uC5F0\uD558\uACE0 \uC7AC\uC0AC\uC6A9 \uAC00\
  \uB2A5\uD55C \uCF54\uB4DC\uB97C \uC791\uC131\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: 2024-02-18 23:09:06.578612
model: gpt-4-1106-preview
summary: "\uBA85\uB839\uC904 \uC778\uC790(command line arguments)\uB97C \uC77D\uB294\
  \ \uAC83\uC740 \uC2A4\uD06C\uB9BD\uD2B8\uC5D0 \uC785\uB825\uC744 \uC804\uB2EC\uD558\
  \uB294 \uBC29\uBC95\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB294 \uC774\
  \uB97C \uD65C\uC6A9\uD574 \uC720\uC5F0\uD558\uACE0 \uC7AC\uC0AC\uC6A9 \uAC00\uB2A5\
  \uD55C \uCF54\uB4DC\uB97C \uC791\uC131\uD569\uB2C8\uB2E4."
title: "\uBA85\uB839\uC904 \uC778\uC218 \uC77D\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)

명령줄 인자(command line arguments)를 읽는 것은 스크립트에 입력을 전달하는 방법입니다. 프로그래머는 이를 활용해 유연하고 재사용 가능한 코드를 작성합니다.

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
