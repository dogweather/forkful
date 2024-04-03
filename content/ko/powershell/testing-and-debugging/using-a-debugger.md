---
date: 2024-01-26 04:09:19.479520-07:00
description: "\uC0AC\uC6A9 \uBC29\uBC95: PowerShell\uC5D0\uC11C\uB294 \uB0B4\uC7A5\
  \uB41C PowerShell \uD1B5\uD569 \uC2A4\uD06C\uB9BD\uD305 \uD658\uACBD(ISE)\uC774\uB098\
  \ PowerShell \uD655\uC7A5 \uAE30\uB2A5\uC744 \uAC00\uC9C4 Visual Studio Code(VS\
  \ Code)\uB97C \uC0AC\uC6A9\uD558\uC5EC \uC2A4\uD06C\uB9BD\uD2B8\uB97C \uB514\uBC84\
  \uAE45\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uB2E4\uC74C\uC740 \uB450 \uD658\uACBD\
  \uC5D0\uC11C \uC911\uB2E8\uC810\uC744 \uC0AC\uC6A9\uD558\uB294\u2026"
lastmod: '2024-03-13T22:44:55.553160-06:00'
model: gpt-4-0125-preview
summary: "PowerShell\uC5D0\uC11C\uB294 \uB0B4\uC7A5\uB41C PowerShell \uD1B5\uD569\
  \ \uC2A4\uD06C\uB9BD\uD305 \uD658\uACBD(ISE)\uC774\uB098 PowerShell \uD655\uC7A5\
  \ \uAE30\uB2A5\uC744 \uAC00\uC9C4 Visual Studio Code(VS Code)\uB97C \uC0AC\uC6A9\
  \uD558\uC5EC \uC2A4\uD06C\uB9BD\uD2B8\uB97C \uB514\uBC84\uAE45\uD560 \uC218 \uC788\
  \uC2B5\uB2C8\uB2E4."
title: "\uB514\uBC84\uAC70 \uC0AC\uC6A9\uD558\uAE30"
weight: 35
---

## 사용 방법:
PowerShell에서는 내장된 PowerShell 통합 스크립팅 환경(ISE)이나 PowerShell 확장 기능을 가진 Visual Studio Code(VS Code)를 사용하여 스크립트를 디버깅할 수 있습니다. 다음은 두 환경에서 중단점을 사용하는 방법입니다:

### PowerShell ISE:
```PowerShell
# 특정 라인에 중단점 설정
Set-PSBreakpoint -Script .\MyScript.ps1 -Line 5

# 스크립트를 정상적으로 실행
.\MyScript.ps1

# 스크립트가 중단점에 도달하면 변수를 검사할 수 있음
$myVariable

# 실행 계속
Continue
```

### Visual Studio Code:
```PowerShell
# VS Code에서 PowerShell 스크립트를 엽니다.
# 라인 번호의 왼쪽을 클릭하여 중단점을 설정합니다.
# F5를 누르거나 '디버깅 시작'을 클릭하여 디버깅을 시작합니다.

# VS Code는 중단점에서 실행을 중지합니다.
# 디버그 패널을 사용하여 변수를 감시하고, 호출 스택을 검사하고, 흐름을 제어하세요.
```

두 환경 모두에서 디버깅을 할 때 단계 진입(F11), 단계 넘김(F10), 그리고 단계 나가기(Shift+F11)를 할 수 있습니다.

## 자세히 살펴보기
역사적으로, PowerShell에서의 디버깅은 다소 불편했고, 변수 상태를 출력하기 위해 많은 `Write-Host` 라인이 필요했거나, 전통적인 시행착오 방법을 필요로 했습니다. PowerShell ISE의 등장, 그리고 더 최근에는 풍부한 디버깅 기능을 갖춘 VS Code로, PowerShell 디버깅은 완전한 프로그래밍 언어만큼 직관적이 되었습니다.

PowerShell의 네이티브 디버깅 도구에 대한 대안으로는 PowerGUI와 같은 타사 도구나 PowerShell 플러그인이 있는 견고한 IDE인 Visual Studio를 사용하는 것입니다.

디버거를 구현할 때는, 특히 점 참조된 스크립트나 모듈을 사용하는 경우 스크립트의 범위를 고려하세요. 중단점은 조건 기반, 변수 변경 기반, 또는 라인 기반일 수 있어, 디버깅 세션 동안 정밀한 제어를 가능하게 합니다.

더욱이, PowerShell Core(크로스 플랫폼 PowerShell)로의 전환으로 디버깅은 대부분 다양한 플랫폼에서 일관된 경험을 제공하는 VS Code에 의해 수행되었습니다.

## 참고자료
PowerShell에서 디버깅에 대한 자세한 내용은:
- [about_Debuggers](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_Debuggers)
