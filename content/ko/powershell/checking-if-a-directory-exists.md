---
title:                "디렉토리가 존재하는지 확인하기"
html_title:           "C#: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 무엇과 왜?

디렉토리 존재 확인은 프로그래머가 파일 시스템에서 특정 디렉토리의 존재 여부를 확인하는 행위입니다. 이는 필사적으로 파일을 저장하거나 디렉토리를 이동하기 전에 오류를 방지하는 매우 중요한 단계입니다.

## 어떻게 해야하는지:

PowerShell에서 디렉토리의 존재 여부를 확인하는 가장 간단한 방법은 Test-Path cmdlet을 사용하는 것입니다: 

```PowerShell
if (Test-Path $path)
{
	Write-Host "Directory Exists"
}
else
{
	Write-Host "Directory Does Not Exist"
}
```
이 코드의 출력 예는 다음과 같습니다:

```
Directory Exists
```
혹은

```
Directory Does Not Exist
```

## 깊은 이해

디렉토리 존재 확인은 파일 시스템에 대한 기초적인 조회이며, 거의 모든 프로그래밍 언어와 환경에서 일반적으로 사용되는 기능입니다. 운영 체제의 파일 시스템 API를 활용해 이 작업을 수행합니다.

대안으로는 `Get-ChildItem` cmdlet을 사용하는 것이 있습니다. 그러나 이 방법은 오직 디렉토리가 존재하면서 해당 디렉토리에 대한 읽기 권한이 있을 때만 작동하므로, `Test-Path` cmdlet을 사용하는 것이 더 안전하고 만능입니다.

기술적으로는, `Test-Path` 함수는 주어진 경로에 대한 파일 시스템 조회를 수행하고, 그 결과를 논리 플래그로 반환합니다.

## 참고 자료:

1. [Test-Path 명령어](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/test-path?view=powershell-7)
2. [Get-ChildItem 명령어](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-childitem?view=powershell-7)
3. [PowerShell 디렉토리 검색](https://ss64.com/ps/syntax-dir.html)