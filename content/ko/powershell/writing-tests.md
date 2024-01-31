---
title:                "테스트 작성하기"
date:                  2024-01-19
html_title:           "Arduino: 테스트 작성하기"
simple_title:         "테스트 작성하기"

tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
테스트 작성이란 코드가 의도한 대로 작동하는지 확인하는 과정입니다. 프로그래머들은 버그를 줄이고, 코드 품질을 보장하며, 나중에 코드를 변경할 때 자신감을 갖기 위해 테스트를 작성합니다.

## How to:
PowerShell에서 간단한 테스트를 작성해보겠습니다. Pester라는 모듈을 사용합시다. 이 예제에서는 `Get-Answer` 함수가 ‘42’라는 올바른 답을 반환하는지 테스트합니다.

```PowerShell
# 설치된 Pester 확인
Get-Module -ListAvailable Pester

# 테스트 파일 작성
@"
function Get-Answer {
    return 42
}

Describe "Get-Answer test" {
    It "returns the correct answer" {
        Get-Answer | Should -Be 42
    }
}
"@ | Out-File -FilePath .\Answer.Tests.ps1

# 테스트 실행
Invoke-Pester .\Answer.Tests.ps1
```

샘플 출력:

```
Describing Get-Answer test
 [+] returns the correct answer 42ms
Tests Completed: 1, Passed: 1, Failed: 0, Skipped: 0 NotRun: 0
```

## Deep Dive:
Pester는 PowerShell에서 테스트를 작성할 수 있도록 하는 프레임워크입니다. 이는 2010년대 초반에 등장했습니다. NUnit과 비슷한 문법을 가진 Pester는 PowerShell의 '상태 검증'에 적합합니다. 복잡한 인프라 코드나 함수를 테스트할 때 유용합니다. 테스트-주도 개발(TDD) 및 지속적 통합(CI) 환경에서 Pester의 중요성은 매우 큽니다. Pester 외에도 다른 대안, 예를 들어 PSUnit, PowerShell에 기반한 다른 테스트 프레임워크도 존재합니다.

## See Also:
- Pester 공식 문서: https://pester.dev/docs/quick-start
- Pester GitHub 저장소: https://github.com/pester/Pester
- PowerShell 테스트에 대한 블로그 게시글: https://devblogs.microsoft.com/scripting/hey-scripting-guy-how-can-i-test-my-powershell-scripts/
