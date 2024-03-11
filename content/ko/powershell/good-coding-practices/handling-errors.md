---
date: 2024-01-26 00:56:28.706408-07:00
description: "PowerShell\uC5D0\uC11C \uC624\uB958\uB97C \uCC98\uB9AC\uD55C\uB2E4\uB294\
  \ \uAC83\uC740 \uC608\uAE30\uCE58 \uC54A\uC740 \uC624\uB958\uB97C \uC608\uCE21\uD558\
  \uACE0 \uC6D0\uD65C\uD558\uAC8C \uAD00\uB9AC\uD558\uB294 \uAC83\uC744 \uC758\uBBF8\
  \uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC774 \uC791\uC5C5\
  \uC744 \uD1B5\uD574 \uC2DC\uC2A4\uD15C \uCDA9\uB3CC\uC744 \uBC29\uC9C0\uD558\uACE0\
  \ \uC0AC\uC6A9\uC790\uC5D0\uAC8C \uC720\uC6A9\uD55C \uD53C\uB4DC\uBC31\uC744 \uC81C\
  \uACF5\uD569\uB2C8\uB2E4."
lastmod: '2024-03-11T00:14:29.481568-06:00'
model: gpt-4-1106-preview
summary: "PowerShell\uC5D0\uC11C \uC624\uB958\uB97C \uCC98\uB9AC\uD55C\uB2E4\uB294\
  \ \uAC83\uC740 \uC608\uAE30\uCE58 \uC54A\uC740 \uC624\uB958\uB97C \uC608\uCE21\uD558\
  \uACE0 \uC6D0\uD65C\uD558\uAC8C \uAD00\uB9AC\uD558\uB294 \uAC83\uC744 \uC758\uBBF8\
  \uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC774 \uC791\uC5C5\
  \uC744 \uD1B5\uD574 \uC2DC\uC2A4\uD15C \uCDA9\uB3CC\uC744 \uBC29\uC9C0\uD558\uACE0\
  \ \uC0AC\uC6A9\uC790\uC5D0\uAC8C \uC720\uC6A9\uD55C \uD53C\uB4DC\uBC31\uC744 \uC81C\
  \uACF5\uD569\uB2C8\uB2E4."
title: "\uC5D0\uB7EC \uCC98\uB9AC\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
PowerShell에서 오류를 처리한다는 것은 예기치 않은 오류를 예측하고 원활하게 관리하는 것을 의미합니다. 프로그래머들은 이 작업을 통해 시스템 충돌을 방지하고 사용자에게 유용한 피드백을 제공합니다.

## 방법:
```PowerShell
# 예외 처리를 위한 기본 Try-Catch
try {
    # 오류를 발생시킬 수 있는 코드
    $result = 1 / 0
} catch {
    # 오류가 발생했을 때 수행할 작업
    Write-Host "이런, 오류가 발생했습니다: $_"
}

# 사용자 정의 오류 메시지 출력
try {
    Get-Item "nonexistentfile.txt" -ErrorAction Stop
} catch {
    Write-Host "파일을 찾을 수 없습니다."
}

# 최근 오류를 검사하기 위한 $Error 변수 사용
```

## 심화 탐구
PowerShell은 Monad로 처음 시작한 이후로 많은 발전을 거듭했습니다. 오류 처리 기능은 시간이 지나며 더욱 견고해졌으며, C#과 같은 다른 프로그래밍 언어들로부터 많은 기능을 가져왔습니다. `try-catch-finally` 문법은 그러한 한 예입니다. 이전에는 스크립터들이 조건을 체크하고 `$Error` 자동 변수를 사용하는 데 주로 의존했습니다.

PowerShell에는 종료 오류와 비종료 오류라는 두 가지 주요 오류 유형이 있습니다. 종료 오류는 `try-catch` 블록에서 캐치하지 않는 이상 스크립트가 중단될 것이며, 비종료 오류는 `-ErrorAction Stop`을 지정하지 않는 한 중단되지 않습니다. 이 구분은 오류 처리에 대해 세밀한 제어를 할 수 있도록 해, 오류가 정말 전체 스크립트를 멈추는 것이 타당한지, 간단히 기록하고 무시할 수 있는지를 결정할 수 있게 합니다.

PowerShell의 오류 처리에서는 무조건 실행되는 `finally` 블록도 사용할 수 있습니다. 오류가 발생한 경우든 아니든 상관없이 실행됩니다. 정리 작업에 아주 적합합니다.

스크립트 작업을 깊이 수행할 때는 특정 예외 유형을 처리하여 더 세밀한 제어를 할 수도 있습니다.

대안으로, 예외를 발생시키지 않고 오류를 캡처하는 구식인 `-ErrorVariable` 매개변수와 마지막 작업이 성공적이었는지 알려주는 `$?` 변수가 있습니다. 훌륭한 도구들이지만, 견고한 `try-catch`만큼은 아닐 수 있습니다.

## 참고 자료
- [about_Try_Catch_Finally](https://docs.microsoft.com/ko-kr/powershell/module/microsoft.powershell.core/about/about_try_catch_finally?view=powershell-7.2)
