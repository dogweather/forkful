---
title:                "표준 에러로 쓰기"
html_title:           "PowerShell: 표준 에러로 쓰기"
simple_title:         "표준 에러로 쓰기"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
표준 오류에 쓰는 것이 무엇인지와 프로그래머들이 그렇게 하는 이유는 무엇인지에 대해 1~2문장으로 설명합니다.

표준 오류(standard error)는 실행 중에 발생하는 오류 메시지와 같은 프로그램 출력물을 말합니다. 프로그래머들은 이 출력물을 디버깅(debugging)하거나 문제를 파악하는 데 사용합니다.

## 하는 법:
```PowerShell
Write-Error "이것은 표준 오류 메시지입니다."
``` 

위의 코드는 표준 오류에 메시지를 쓰는 간단한 예제입니다.
실행시키면 다음과 같은 출력이 나타납니다:
```PowerShell
이것은 표준 오류 메시지입니다.
```
더 자세한 예제를 보려면 다음 명령어를 사용할 수 있습니다:
```PowerShell
Get-Help Write-Error -Examples
```

## 더 자세히 살펴보기:
표준 오류를 기록하는 것은 프로그램 실행 중 문제가 발생했을 때 유용합니다. 디버깅을 할 때, 어떤 오류가 발생했는지를 알 수 있고, 오류 메시지를 적절하게 처리할 수 있습니다.

표준 오류를 기록하는 기능은 다른 프로그래밍 언어들에서도 비슷하게 사용됩니다. 예를 들어, JavaScript에서는 `console.error()` 함수를 사용하고, C++에서는 `cerr` 객체를 사용합니다.

PowerShell에서 표준 오류를 기록할 때, `Write-Error` 명령어를 사용하며, `-ErrorAction` 파라미터를 사용하여 출력을 다르게 할 수 있습니다. `-ErrorAction` 파라미터에 `Stop` 값을 입력하면, 오류 메시지를 콘솔 화면에 출력하고 스크립트를 중지합니다.

## 관련 자료:
- [PowerShell Write-Error](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/write-error?view=powershell-7.1)
- [Debugging Scripts with Write-Error](https://docs.microsoft.com/en-us/powershell/scripting/samples/debugging-scripts-with-write-error?view=powershell-7.1)
- [PowerShell Error Handling](https://docs.microsoft.com/en-us/powershell/scripting/diagnostics/psake/handling?view=powershell-7.1)