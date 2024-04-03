---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:20.641753-07:00
description: "\uBC29\uBC95: \uD30C\uC6CC\uC178\uC740 `Test-Path` cmdlet\uC744 \uC0AC\
  \uC6A9\uD558\uC5EC \uB514\uB809\uD1A0\uB9AC\uC758 \uC874\uC7AC \uC5EC\uBD80\uB97C\
  \ \uAC04\uB2E8\uD788 \uD655\uC778\uD560 \uC218 \uC788\uB294 \uBC29\uBC95\uC744 \uC81C\
  \uACF5\uD569\uB2C8\uB2E4. \uC774 cmdlet\uC740 \uC9C0\uC815\uB41C \uACBD\uB85C\uAC00\
  \ \uC874\uC7AC\uD558\uB294\uC9C0 \uC5EC\uBD80\uB97C \uB098\uD0C0\uB0B4\uB294 \uBD80\
  \uC6B8 \uAC12(Boolean value)\uC744 \uBC18\uD658\uD569\uB2C8\uB2E4. \uC5EC\uAE30\
  \ \uADF8 \uC0AC\uC6A9 \uBC29\uBC95\uC774 \uC788\uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.568075-06:00'
model: gpt-4-0125-preview
summary: "\uD30C\uC6CC\uC178\uC740 `Test-Path` cmdlet\uC744 \uC0AC\uC6A9\uD558\uC5EC\
  \ \uB514\uB809\uD1A0\uB9AC\uC758 \uC874\uC7AC \uC5EC\uBD80\uB97C \uAC04\uB2E8\uD788\
  \ \uD655\uC778\uD560 \uC218 \uC788\uB294 \uBC29\uBC95\uC744 \uC81C\uACF5\uD569\uB2C8\
  \uB2E4."
title: "\uB514\uB809\uD1A0\uB9AC\uAC00 \uC874\uC7AC\uD558\uB294\uC9C0 \uD655\uC778\
  \uD558\uAE30"
weight: 20
---

## 방법:
파워셸은 `Test-Path` cmdlet을 사용하여 디렉토리의 존재 여부를 간단히 확인할 수 있는 방법을 제공합니다. 이 cmdlet은 지정된 경로가 존재하는지 여부를 나타내는 부울 값(Boolean value)을 반환합니다. 여기 그 사용 방법이 있습니다:

```powershell
# 디렉토리가 존재하는지 확인
$directoryPath = "C:\ExamplePath"
$directoryExists = Test-Path -Path $directoryPath
Write-Output "디렉토리가 존재하나요? $directoryExists"
```

존재하는 디렉터리에 대한 샘플 출력:

```
디렉토리가 존재하나요? True
```

존재하지 않는 디렉터리에 대한 샘플 출력:

```
디렉토리가 존재하나요? False
```

네트워크 공유나 클라우드 스토리지와 상호작용하는 등 더 복잡한 스크립트의 경우, `Test-Path`로 직접 제공되지 않는 추가적인 검사나 기능이 필요할 수 있습니다. 이러한 경우, 제3자 파워셸 모듈이나 라이브러리를 활용하는 것이 유익할 수 있습니다. 그럼에도 불구하고 대부분의 일상적인 작업은 파워셸의 기본 cmdlet으로 해결될 수 있습니다. 마지막으로 제 지식이 업데이트된 시점까지, `Test-Path`가 제공하는 것을 넘어서 디렉토리 존재 여부를 확인하기 위한 널리 채택된 제3자 라이브러리는 없었습니다. 주로 `Test-Path` 자체가 이 목적에 대해 강력하고 효율적이기 때문입니다.
