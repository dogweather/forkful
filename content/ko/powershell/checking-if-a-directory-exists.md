---
title:                "디렉토리 존재 여부 확인하기"
date:                  2024-01-20T14:58:11.957446-07:00
html_title:           "Fish Shell: 디렉토리 존재 여부 확인하기"
simple_title:         "디렉토리 존재 여부 확인하기"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
디렉토리가 존재하는지 확인하는 것은 파일 시스템에서 폴더가 실제로 있는지 검사하는 과정입니다. 프로그래머들은 불필요한 오류를 피하고, 데이터를 정확한 장소에 저장하며, 스크립트가 예상대로 작동하도록 하기 위해 이 작업을 합니다.

## How to: (방법:)
PowerShell에서 디렉토리 존재 여부를 확인하는 기본적인 방법입니다:

```PowerShell
# 디렉토리 경로 설정
$directoryPath = "C:\Some\Directory"

# 디렉토리 존재 여부 확인
if (Test-Path $directoryPath -PathType Container) {
    Write-Host "$directoryPath exists."
} else {
    Write-Host "$directoryPath does not exist."
}
```

실행 결과:
```
C:\Some\Directory exists.
```
또는
```
C:\Some\Directory does not exist.
```

## Deep Dive (깊이 들어가기)
`Test-Path` cmdlet은 PowerShell 버전 1.0부터 사용할 수 있으며, 파일이나 디렉토리가 존재하는지 확인하는 데 사용됩니다. `-PathType Container` 옵션을 사용하면 디렉토리(컨테이너)만을 대상으로 합니다. 파일을 확인하려면 `-PathType Leaf`를 사용합니다. 속성 `-LiteralPath`는 와일드카드 문자를 해석하지 않고 경로를 그대로 사용할 때 사용할 수 있습니다. 이 cmdlet은 변수, 환경 변수, 레지스트리 키에서도 사용 가능하여 PowerShell 스크립트에서 매우 다양하게 활용됩니다.

## See Also (더 보기)
- [About Test-Path (영문)](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/test-path)
