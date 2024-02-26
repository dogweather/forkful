---
date: 2024-01-26 04:16:52.796677-07:00
description: "\uC778\uD130\uB799\uD2F0\uBE0C \uC178, \uB610\uB294 Read-Eval-Print\
  \ Loop (REPL)\uC740 PowerShell \uBA85\uB839\uC5B4\uB97C \uC785\uB825\uD558\uACE0\
  \ \uC989\uAC01\uC801\uC778 \uD53C\uB4DC\uBC31\uC744 \uBC1B\uC744 \uC218 \uC788\uAC8C\
  \ \uD574\uC90D\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC774\uB97C\
  \ \uD1B5\uD574 \uCF54\uB4DC \uC870\uAC01\uC744 \uBE60\uB974\uAC8C \uD14C\uC2A4\uD2B8\
  \uD558\uAC70\uB098, \uB514\uBC84\uAE45\uD558\uAC70\uB098, \uC804\uCCB4 \uC2A4\uD06C\
  \uB9BD\uD2B8\uB97C \uC791\uC131\uD558\uC9C0 \uC54A\uACE0 \uC0C8 \uBA85\uB839\uC5B4\
  \uB97C \uD559\uC2B5\uD560\u2026"
lastmod: '2024-02-25T18:49:52.539465-07:00'
model: gpt-4-0125-preview
summary: "\uC778\uD130\uB799\uD2F0\uBE0C \uC178, \uB610\uB294 Read-Eval-Print Loop\
  \ (REPL)\uC740 PowerShell \uBA85\uB839\uC5B4\uB97C \uC785\uB825\uD558\uACE0 \uC989\
  \uAC01\uC801\uC778 \uD53C\uB4DC\uBC31\uC744 \uBC1B\uC744 \uC218 \uC788\uAC8C \uD574\
  \uC90D\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC774\uB97C \uD1B5\
  \uD574 \uCF54\uB4DC \uC870\uAC01\uC744 \uBE60\uB974\uAC8C \uD14C\uC2A4\uD2B8\uD558\
  \uAC70\uB098, \uB514\uBC84\uAE45\uD558\uAC70\uB098, \uC804\uCCB4 \uC2A4\uD06C\uB9BD\
  \uD2B8\uB97C \uC791\uC131\uD558\uC9C0 \uC54A\uACE0 \uC0C8 \uBA85\uB839\uC5B4\uB97C\
  \ \uD559\uC2B5\uD560\u2026"
title: "\uC778\uD130\uB799\uD2F0\uBE0C \uC178 (REPL) \uC0AC\uC6A9\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
인터랙티브 셸, 또는 Read-Eval-Print Loop (REPL)은 PowerShell 명령어를 입력하고 즉각적인 피드백을 받을 수 있게 해줍니다. 프로그래머들은 이를 통해 코드 조각을 빠르게 테스트하거나, 디버깅하거나, 전체 스크립트를 작성하지 않고 새 명령어를 학습할 수 있습니다.

## 방법:
PowerShell을 실행하면 REPL에 진입하게 됩니다. `Get-Date` Cmdlet을 시도해보세요:

```PowerShell
PS > Get-Date
```

현재 날짜와 시간 출력을 보게 될 것입니다:

```PowerShell
2023년 3월 31일 수요일 오후 12:34:56
```

이제, 명령어를 연결해봅시다. 메모리 사용량별로 프로세스를 정렬해보겠습니다:

```PowerShell
PS > Get-Process | Sort-Object WS -Descending | Select-Object -First 5
```

이는 작업 세트 크기(메모리 사용량)별로 상위 5개 프로세스를 출력합니다.

## 깊은 탐색
PowerShell의 REPL은 Unix 셸과 Python과 같은 다른 동적 언어 셸의 뿌리를 가지고 있습니다. 이는 단일 사용자, 상호 작용적 명령 실행 환경입니다. 컴파일 언어와 달리 프로그램 전체를 작성한 다음 컴파일하는 대신, REPL 환경은 한 줄씩 코드를 작성하고 실행할 수 있게 해줍니다. PowerShell은 또한 더 큰 작업을 위한 스크립트 실행도 지원합니다.

Windows에 대한 대안으로는 명령 프롬프트나 IPython과 같은 언어 특화 REPL들이 있습니다. Unix/Linux 세계에서는 bash나 zsh 같은 셸이 비슷한 기능을 제공합니다.

PowerShell의 구현은 호스트 애플리케이션을 사용하여 셸을 실행합니다. Windows에서 가장 흔한 PowerShell.exe를 비롯해, 통합 스크립트 환경(ISE)이나 Visual Studio Code의 통합 터미널도 호스트로서 기능할 수 있습니다.

## 참고
- [PowerShell 정보](https://docs.microsoft.com/en-us/powershell/scripting/overview)
- [StackOverflow: PowerShell](https://stackoverflow.com/questions/tagged/powershell)
