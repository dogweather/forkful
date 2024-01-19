---
title:                "디버그 출력을 인쇄하기"
html_title:           "Clojure: 디버그 출력을 인쇄하기"
simple_title:         "디버그 출력을 인쇄하기"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/printing-debug-output.md"
---

{{< edit_this_page >}}

## 뭐 & 왜?
프로그래머들이 디버그 출력을 하는 이유는 코드의 개별 행동을 볼 수 있기 때문입니다. 즉시 수정하고 자신의 가정을 테스트할 수 있습니다.

## 이렇게 해요:
우리가 디버그를 출력하려면 PowerShell에서는 `Write-Debug` 명령어를 사용해요.

```PowerShell
$DebugPreference = 'Continue'
Write-Debug "This is a debug message."
```
아웃풋:

```PowerShell
DEBUG: This is a debug message. 
```
이 코드는 디버그 메시지를 콘솔에 출력합니다. 'Continue'는 디버그 메시지를 출력할 것인지를 결정하는 변수입니다.

## Deep Dive
디버그 출력은 프로그래밍의 오래된 조언 중 하나입니다. PowerShell에서는 `Write-Verbose` 또는 `Write-Warning`으로 메시지 유형을 변경할 수 있습니다. `$DebugPreference` 환경 변수는 디버그 메서드의 출력을 관리합니다. 'SilentlyContinue'를 사용하면 출력을 억제할 수 있습니다.

```PowerShell
$DebugPreference = 'SilentlyContinue'
Write-Debug "This message won't be printed."
```
## 참고하셔요:
- PowerShell debugging (https://docs.microsoft.com/en-us/powershell/scripting/learn/debugging)
- About Preference Variables (https://docs.microsoft.com/ko-kr/powershell/module/microsoft.powershell.core/about/about_preference_variables)