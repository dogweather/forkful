---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:15.163773-07:00
description: "PowerShell\uC5D0\uC11C \uD45C\uC900 \uC624\uB958(stderr)\uC5D0 \uC4F0\
  \uAE30\uB294 \uC624\uB958 \uBA54\uC2DC\uC9C0\uB098 \uC9C4\uB2E8\uC744 \uD45C\uC900\
  \ \uCD9C\uB825(stdout) \uC2A4\uD2B8\uB9BC\uACFC \uAD6C\uBCC4\uB418\uB294 stderr\
  \ \uC2A4\uD2B8\uB9BC\uC5D0 \uC9C1\uC811 \uBCF4\uB0B4\uB294 \uAC83\uC744 \uD3EC\uD568\
  \uD569\uB2C8\uB2E4. \uC774 \uBD84\uB9AC\uB294 \uC2A4\uD06C\uB9BD\uD2B8\uC758 \uCD9C\
  \uB825\uC744 \uB354 \uC815\uD655\uD558\uAC8C \uC81C\uC5B4\uD560 \uC218 \uC788\uAC8C\
  \ \uD574\uC8FC\uC5B4 \uAC1C\uBC1C\uC790\uAC00 \uC815\uC0C1 \uBA54\uC2DC\uC9C0\uC640\
  \ \uC624\uB958 \uBA54\uC2DC\uC9C0\uB97C\u2026"
lastmod: '2024-02-25T18:49:52.560680-07:00'
model: gpt-4-0125-preview
summary: "PowerShell\uC5D0\uC11C \uD45C\uC900 \uC624\uB958(stderr)\uC5D0 \uC4F0\uAE30\
  \uB294 \uC624\uB958 \uBA54\uC2DC\uC9C0\uB098 \uC9C4\uB2E8\uC744 \uD45C\uC900 \uCD9C\
  \uB825(stdout) \uC2A4\uD2B8\uB9BC\uACFC \uAD6C\uBCC4\uB418\uB294 stderr \uC2A4\uD2B8\
  \uB9BC\uC5D0 \uC9C1\uC811 \uBCF4\uB0B4\uB294 \uAC83\uC744 \uD3EC\uD568\uD569\uB2C8\
  \uB2E4. \uC774 \uBD84\uB9AC\uB294 \uC2A4\uD06C\uB9BD\uD2B8\uC758 \uCD9C\uB825\uC744\
  \ \uB354 \uC815\uD655\uD558\uAC8C \uC81C\uC5B4\uD560 \uC218 \uC788\uAC8C \uD574\uC8FC\
  \uC5B4 \uAC1C\uBC1C\uC790\uAC00 \uC815\uC0C1 \uBA54\uC2DC\uC9C0\uC640 \uC624\uB958\
  \ \uBA54\uC2DC\uC9C0\uB97C\u2026"
title: "\uD45C\uC900 \uC5D0\uB7EC\uC5D0 \uC4F0\uAE30"
---

{{< edit_this_page >}}

## 무엇이며 왜인가?

PowerShell에서 표준 오류(stderr)에 쓰기는 오류 메시지나 진단을 표준 출력(stdout) 스트림과 구별되는 stderr 스트림에 직접 보내는 것을 포함합니다. 이 분리는 스크립트의 출력을 더 정확하게 제어할 수 있게 해주어 개발자가 정상 메시지와 오류 메시지를 다른 목적지로 직접 보낼 수 있도록 합니다. 이것은 오류 처리와 로깅에 있어 기본적인 것입니다.

## 어떻게:

PowerShell은 `Write-Error` cmdlet 사용이나 `$host.ui.WriteErrorLine()` 메소드로 출력을 지시하는 것을 통해 stderr에 쓰기 과정을 단순화합니다. 그러나 직접적인 stderr 리디렉션을 위해서는 .NET 메소드나 PowerShell 자체가 제공하는 파일 설명자 리디렉션을 선호할 수 있습니다.

**예제 1:** `Write-Error`를 사용하여 stderr에 오류 메시지를 쓰기.

```powershell
Write-Error "This is an error message."
```

stderr 출력:
```
Write-Error: This is an error message.
```

**예제 2:** `$host.ui.WriteErrorLine()`를 사용하여 직접 stderr에 쓰기.

```powershell
$host.ui.WriteErrorLine("Direct stderr write.")
```

stderr 출력:
```
Direct stderr write.
```

**예제 3:** .NET 메소드를 사용하여 stderr에 쓰기.

```powershell
[Console]::Error.WriteLine("Using .NET method for stderr")
```

이 메소드의 출력:
```
Using .NET method for stderr
```

**예제 4:** 파일 설명자 `2>`를 사용한 오류 출력 리디렉션.

PowerShell에서는 파일 설명자가 다양한 스트림을 리디렉션할 수 있습니다. stderr의 경우 파일 설명자는 `2`입니다. 여기에 오류를 생성하는 명령을 실행하는 동안 stderr를 `error.log`라는 파일로 리디렉션하는 예가 있습니다.

```powershell
Get-Item NonExistentFile.txt 2> error.log
```

이 예제는 콘솔 출력을 생성하지 않지만, 존재하지 않는 파일에 접근하려고 시도할 때의 오류 메시지를 포함하는 현재 디렉터리의 `error.log` 파일을 생성합니다.

결론적으로, PowerShell은 스크립트와 애플리케이션에서 세련된 오류 처리 및 로깅 전략을 가능하게 하는 효과적인 오류 출력 쓰기 및 관리 방법을 여러 가지 제공합니다.
