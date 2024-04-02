---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:20.641753-07:00
description: "\uD30C\uC6CC\uC178\uC5D0\uC11C \uB514\uB809\uD1A0\uB9AC\uAC00 \uC874\
  \uC7AC\uD558\uB294\uC9C0 \uD655\uC778\uD558\uB294 \uAC83\uC740 \uD30C\uC77C\uC2DC\
  \uC2A4\uD15C \uAD6C\uC870\uC5D0 \uAE30\uBC18\uD55C \uC758\uC0AC \uACB0\uC815\uC744\
  \ \uB3D5\uAE30 \uC704\uD574 \uC2A4\uD06C\uB9BD\uD2B8\uC5D0\uC11C \uD754\uD788 \uC218\
  \uD589\uD558\uB294 \uC791\uC5C5\uC785\uB2C8\uB2E4. \uC608\uB97C \uB4E4\uC5B4, \uC77D\
  \uAE30 \uB610\uB294 \uC4F0\uAE30 \uC791\uC5C5\uC744 \uC2DC\uB3C4\uD558\uAE30 \uC804\
  \uC5D0 \uB300\uC0C1 \uB514\uB809\uD1A0\uB9AC\uAC00 \uC81C\uC790\uB9AC\uC5D0 \uC788\
  \uB294\uC9C0 \uD655\uC778\uD568\uC73C\uB85C\uC368 \uC624\uB958\uB97C \uBC29\uC9C0\
  \uD558\uB294 \uAC83\uC785\uB2C8\uB2E4. \uB2E4\uC591\uD55C \uD658\uACBD\uC5D0\uC11C\
  \u2026"
lastmod: '2024-03-13T22:44:55.568075-06:00'
model: gpt-4-0125-preview
summary: "\uD30C\uC6CC\uC178\uC5D0\uC11C \uB514\uB809\uD1A0\uB9AC\uAC00 \uC874\uC7AC\
  \uD558\uB294\uC9C0 \uD655\uC778\uD558\uB294 \uAC83\uC740 \uD30C\uC77C\uC2DC\uC2A4\
  \uD15C \uAD6C\uC870\uC5D0 \uAE30\uBC18\uD55C \uC758\uC0AC \uACB0\uC815\uC744 \uB3D5\
  \uAE30 \uC704\uD574 \uC2A4\uD06C\uB9BD\uD2B8\uC5D0\uC11C \uD754\uD788 \uC218\uD589\
  \uD558\uB294 \uC791\uC5C5\uC785\uB2C8\uB2E4. \uC608\uB97C \uB4E4\uC5B4, \uC77D\uAE30\
  \ \uB610\uB294 \uC4F0\uAE30 \uC791\uC5C5\uC744 \uC2DC\uB3C4\uD558\uAE30 \uC804\uC5D0\
  \ \uB300\uC0C1 \uB514\uB809\uD1A0\uB9AC\uAC00 \uC81C\uC790\uB9AC\uC5D0 \uC788\uB294\
  \uC9C0 \uD655\uC778\uD568\uC73C\uB85C\uC368 \uC624\uB958\uB97C \uBC29\uC9C0\uD558\
  \uB294 \uAC83\uC785\uB2C8\uB2E4. \uB2E4\uC591\uD55C \uD658\uACBD\uC5D0\uC11C\u2026"
title: "\uB514\uB809\uD1A0\uB9AC\uAC00 \uC874\uC7AC\uD558\uB294\uC9C0 \uD655\uC778\
  \uD558\uAE30"
weight: 20
---

## 무엇과 왜?
파워셸에서 디렉토리가 존재하는지 확인하는 것은 파일시스템 구조에 기반한 의사 결정을 돕기 위해 스크립트에서 흔히 수행하는 작업입니다. 예를 들어, 읽기 또는 쓰기 작업을 시도하기 전에 대상 디렉토리가 제자리에 있는지 확인함으로써 오류를 방지하는 것입니다. 다양한 환경에서 스크립트가 신뢰성 있게 동작하도록 하는 것이 필수입니다.

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
