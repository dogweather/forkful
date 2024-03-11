---
date: 2024-01-26 04:09:19.479520-07:00
description: "\uB514\uBC84\uAC70\uB97C \uC0AC\uC6A9\uD55C\uB2E4\uB294 \uAC83\uC740\
  \ \uC911\uB2E8\uC810\uC744 \uC124\uC815\uD558\uACE0, \uCF54\uB4DC\uB97C \uB2E8\uACC4\
  \uBCC4\uB85C \uC2E4\uD589\uD558\uBA70, \uBCC0\uC218\uB97C \uD655\uC778\uD558\uACE0\
  \ \uD504\uB85C\uADF8\uB7A8\uC758 \uC0C1\uD0DC\uB97C \uAC80\uC0AC\uD558\uB294 \uAC83\
  \uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4. \uC774\uB294 \uD504\uB85C\uADF8\uB798\uBA38\
  \uC5D0\uAC8C \uAC8C\uC784 \uCCB4\uC778\uC800\uC774\uBA70, \uBC84\uADF8\uB97C \uCC3E\
  \uC544\uC8FC\uACE0 \uC6B0\uB9AC\uC758 \uCF54\uB4DC\uAC00 \uC2E4\uC81C\uB85C \uBB34\
  \uC5C7\uC744 \uD558\uACE0 \uC788\uB294\uC9C0 \uC774\uD574\uD558\uB294 \uB370 \uB3C4\
  \uC6C0\uC774 \uB429\uB2C8\uB2E4."
lastmod: '2024-03-11T00:14:29.477085-06:00'
model: gpt-4-0125-preview
summary: "\uB514\uBC84\uAC70\uB97C \uC0AC\uC6A9\uD55C\uB2E4\uB294 \uAC83\uC740 \uC911\
  \uB2E8\uC810\uC744 \uC124\uC815\uD558\uACE0, \uCF54\uB4DC\uB97C \uB2E8\uACC4\uBCC4\
  \uB85C \uC2E4\uD589\uD558\uBA70, \uBCC0\uC218\uB97C \uD655\uC778\uD558\uACE0 \uD504\
  \uB85C\uADF8\uB7A8\uC758 \uC0C1\uD0DC\uB97C \uAC80\uC0AC\uD558\uB294 \uAC83\uC744\
  \ \uC758\uBBF8\uD569\uB2C8\uB2E4. \uC774\uB294 \uD504\uB85C\uADF8\uB798\uBA38\uC5D0\
  \uAC8C \uAC8C\uC784 \uCCB4\uC778\uC800\uC774\uBA70, \uBC84\uADF8\uB97C \uCC3E\uC544\
  \uC8FC\uACE0 \uC6B0\uB9AC\uC758 \uCF54\uB4DC\uAC00 \uC2E4\uC81C\uB85C \uBB34\uC5C7\
  \uC744 \uD558\uACE0 \uC788\uB294\uC9C0 \uC774\uD574\uD558\uB294 \uB370 \uB3C4\uC6C0\
  \uC774 \uB429\uB2C8\uB2E4."
title: "\uB514\uBC84\uAC70 \uC0AC\uC6A9\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
디버거를 사용한다는 것은 중단점을 설정하고, 코드를 단계별로 실행하며, 변수를 확인하고 프로그램의 상태를 검사하는 것을 의미합니다. 이는 프로그래머에게 게임 체인저이며, 버그를 찾아주고 우리의 코드가 실제로 무엇을 하고 있는지 이해하는 데 도움이 됩니다.

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
