---
title:                "디렉토리가 존재하는지 확인하는 방법"
html_title:           "PowerShell: 디렉토리가 존재하는지 확인하는 방법"
simple_title:         "디렉토리가 존재하는지 확인하는 방법"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 무슨 이야기인가요?

디렉토리가 존재하는지 확인하는 것은 프로그래머들이 코드를 작성할 때 자주 하는 작업 중 하나입니다. 이를테면, 특정 디렉토리가 존재하는지 확인하고 없을 경우 새로 생성하는 등의 작업을 수행할 수 있기 때문입니다.

이 작업을 수행하는 이유는 일반적으로 프로그램이나 스크립트를 실행하는 과정에서 사용자로부터 입력받은 경로나 파일이 존재하는지 먼저 확인하는 것이 일반적입니다. 이를 통해 오류를 방지하고 보다 안정적인 프로그램을 만들 수 있습니다.

## 방법은?

```PowerShell
if (Test-Path "C:\Users\Documents\TestFolder") {
    Write-Host "The directory exists!"
}
else {
    New-Item -Path "C:\Users\Documents\TestFolder" -ItemType Directory
    Write-Host "The directory was created."
}
```

위 코드는 "C:\Users\Documents\TestFolder" 경로에 해당하는 디렉토리가 존재하는지 확인한 뒤, 존재한다면 "The directory exists!"를 출력하고, 존재하지 않는다면 해당 경로에 새로운 디렉토리를 생성하고 "The directory was created."를 출력합니다.

## 더 깊게 들어가보면?

디렉토리가 존재하는지 확인하는 작업은 오래 전부터 사용되어왔습니다. 이전에는 프로그래밍 언어의 일부로 포함되어 있었지만, PowerShell과 같은 스크립팅 언어에서는 외부 툴인 Test-Path 함수를 사용하여 단순하게 구현할 수 있도록 제공됩니다.

만약 디렉토리를 확인하는 것이 아니라 파일을 확인하고 싶다면, 파일을 찾는 것과 비슷한 방법으로 코드를 작성할 수 있습니다. 예를 들어, "TestFile.txt" 파일이 존재하는지 확인하고 싶은 경우에는 "Test-Path" 뒤의 경로를 "C:\Users\Documents\TestFile.txt"로 바꿔주면 됩니다.

추가적으로, "Test-Path" 함수는 디렉토리 뿐만 아니라 레지스트리 항목이나 특정 원격 서버 경로를 확인하는데에도 사용할 수 있습니다.

## 관련 자료

- [PowerShell - Test-Path 함수](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/test-path?view=powershell-7.1)
- [PowerShell - Directory 만들기](https://github.com/gmin/PS-Kor-by-Min/blob/master/mk-dir.ps1)