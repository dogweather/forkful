---
title:                "명령줄 인수 읽기"
html_title:           "Arduino: 명령줄 인수 읽기"
simple_title:         "명령줄 인수 읽기"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜? 와 무엇?

명령행 인자 읽기는 프로그램이 명령행에서 입력을 받아들이는 과정입니다. 프로그래머들이 이것을 하는 이유는 사용자 정의 입력을 쉽게 처리하고 특정 동작을 자동화하는 데 유용하기 때문입니다.

## 어떻게:

```PowerShell
# 입력 인수 출력
param (
   [string[]]$arguments
)

foreach ($arg in $arguments)
{
    Write-Output $arg
}
```
해당 코드를 myscript.ps1로 저장하고, PowerShell에서 다음 명령을 실행합니다.
```PowerShell
.\myscript.ps1 인수1 인수2 인수3
```

출력 결과:
```
인수1
인수2
인수3
```

이 예시에서, 명령행 인자로 넘긴 세 개의 인자가 출력됩니다.

## 깊게 알아보기:

명령행 인자 읽기는 초기 유닉스 셸 스크립트와 최초의 C 프로그램 언어에서 유래되었습니다. 그 이후,이 개념은 거의 모든 현대적인 프로그래밍 언어에 적용되었습니다.

PowerShell 같은 스크립트 언어는 넓은 범위의 사용을위해 이러한 입력 인수를 쉽게 노출합니다. 

단, PowerShell 매개변수는 기본적으로 하나의 값만 허용하지만, `[string[]]$arguments`와 같은 배열을 선언함으로써 여러 개의 값이 데이터 형식으로 수신 될 수 있습니다.

## 더 찾아보기:

- Stack Overflow: [How can I read command line parameters from an alias?](https://stackoverflow.com/questions/8475232/how-can-i-read-command-line-parameters-from-an-alias)