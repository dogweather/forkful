---
date: 2024-01-26 04:16:52.796677-07:00
description: "\uBC29\uBC95: PowerShell\uC744 \uC2E4\uD589\uD558\uBA74 REPL\uC5D0 \uC9C4\
  \uC785\uD558\uAC8C \uB429\uB2C8\uB2E4. `Get-Date` Cmdlet\uC744 \uC2DC\uB3C4\uD574\
  \uBCF4\uC138\uC694."
lastmod: '2024-03-13T22:44:55.548939-06:00'
model: gpt-4-0125-preview
summary: "PowerShell\uC744 \uC2E4\uD589\uD558\uBA74 REPL\uC5D0 \uC9C4\uC785\uD558\uAC8C\
  \ \uB429\uB2C8\uB2E4."
title: "\uC778\uD130\uB799\uD2F0\uBE0C \uC178 (REPL) \uC0AC\uC6A9\uD558\uAE30"
weight: 34
---

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
