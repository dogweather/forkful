---
title:                "명령어 줄 인수 읽기"
html_title:           "PowerShell: 명령어 줄 인수 읽기"
simple_title:         "명령어 줄 인수 읽기"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

커맨드 라인 인수를 읽는 것은 프로그래머가 사용자의 입력을 이해하고 기대에 부합하는 작업을 수행하기 위해 필요합니다. 예를 들어, 사용자가 실행할 때마다 다른 파일을 처리할 수 있도록 유연성을 제공합니다.

## 하는 방법:

```PowerShell
$arguments = $args # 사용자 입력을 변수에 저장합니다. 
foreach ($arg in $arguments) # 인수를 하나씩 반복합니다. 
{ 
    Write-Host "Hello, $arg" # 각 인수를 사용하여 쓰기 작업을 수행합니다. 
} 
```

**출력:**

> Hello, [첫번째 인수]
> Hello, [두번째 인수]
> ...


```PowerShell
Param (
    [Parameter(Mandatory)] # 사용자가 반드시 입력해야 하는 인수에 적용되는 특성입니다.
    [String]$name, # 이름 인수를 선언합니다. 
        
    [String]$language = "PowerShell" # 기본값을 설정하는 인수입니다.
)
Write-Host "Hello, $name! Welcome to $language!" # 이름과 언어를 사용한 메시지를 출력합니다.
```

**출력:**

> Hello, [이름]! Welcome to PowerShell!

## 깊게 파헤치기:

- 커맨드 라인 인수는 명령줄 인터페이스의 일부로서 오래전부터 사용해 왔습니다.
- 대부분의 프로그래밍 언어에서 커맨드 라인 인수를 읽고 처리할 수 있는 기능을 제공합니다. 
- `Param` 키워드를 사용하여 인수를 선언할 수 있습니다. 이를 통해 인수의 유형, 필수 여부 및 기본값을 지정할 수 있습니다.

## 관련 자료:

- [PowerShell 공식 문서] (https://docs.microsoft.com/en-us/powershell/scripting/overview?view=powershell-7)
- [PowerShell 코딩 스타일 가이드] (https://github.com/PoshCode/PowerShellPracticeAndStyle)
- [PowerShell 인수 관리] (https://kevinmarquette.github.io/2016-11-06-powershell-argument-validation/#advanced)