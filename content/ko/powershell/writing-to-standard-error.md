---
title:                "표준 오류로 쓰기"
date:                  2024-01-19
simple_title:         "표준 오류로 쓰기"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며, 왜 사용하는가?)
표준 오류란, 스크립트나 프로그램이 오류 메시지를 출력하는 데 사용하는 출력 스트림이다. 프로그래머는 오류를 기록하고, 일반 출력과 구분하여 문제 해결에 도움이 되게 하기 위해 이것을 사용한다.

## How to: (사용 방법)
PowerShell에서 표준 오류에 쓰기:
```PowerShell
# 오류 메시지를 표준 오류로 직접 보내기
Write-Error "This is a sample error message." 2>&1

# 예외 발생시키기
throw "This will go to standard error."

# try/catch 블록을 사용하여 오류를 잡고 표준 오류에 쓰기
try {
    Get-Item "nonexistentfile.txt"
} catch {
    Write-Error $_.Exception.Message
}
```
샘플 출력:
```
Write-Error: This is a sample error message.
This will go to standard error.
Get-Item: Cannot find path 'C:\nonexistentfile.txt' because it does not exist.
```

## Deep Dive (심층 분석)
과거에는 표준 오류를 다루는 것이 오늘날처럼 직관적이지 않았다. PowerShell 2.0 이전 버전에서는 `2>&1`을 사용하여 표준 오류를 표준 출력으로 리다이렉션해야만 했다. 이제는 `Write-Error`나 `throw` 명령어를 활용해 오류 메시지를 쉽게 작성할 수 있다. 이러한 방법 이외에도, `Write-Host`를 사용하여 특정 색상의 텍스트로 오류 메시지를 표시할 수도 있다(사용하지 않는 것이 좋지만). 표준 오류에 문자열을 작성하면, 이 스트림을 모니터링하는 모니터링 소프트웨어나 로그 파일로 쉽게 리다이렉트할 수 있다.

## See Also (참고 자료)
- [About Automatic Variables](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_automatic_variables?view=powershell-7.1)
- [About Try, Catch, and Finally](https://docs.microsoft.com/en-us/powershell/scripting/learn/deep-dives/everything-about-exceptions?view=powershell-7.1)
- [About_Redirection](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_redirection?view=powershell-7.1)
