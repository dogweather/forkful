---
title:                "디렉토리가 존재하는지 확인하기"
aliases:
- /ko/powershell/checking-if-a-directory-exists.md
date:                  2024-02-03T19:08:20.641753-07:00
model:                 gpt-4-0125-preview
simple_title:         "디렉토리가 존재하는지 확인하기"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
