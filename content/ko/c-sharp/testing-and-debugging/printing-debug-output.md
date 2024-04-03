---
date: 2024-01-20 17:52:31.906330-07:00
description: "How to: (\uBC29\uBC95) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.237388-06:00'
model: gpt-4-1106-preview
summary: .
title: "\uB514\uBC84\uADF8 \uCD9C\uB825\uC744 \uCC0D\uC5B4\uBCF4\uAE30"
weight: 33
---

## How to: (방법)
```csharp
using System;

class Program
{
    static void Main()
    {
        // 콘솔에 메시지 출력
        Console.WriteLine("Hello, Debug!");

        // 변수 값 출력
        int answer = 42;
        Console.WriteLine("The answer is " + answer);

        // 조건부 디버그 출력 - Release 모드에서는 나타나지 않음
        System.Diagnostics.Debug.WriteLine("This will only show in Debug mode.");
    }
}
```
예상 출력:
```
Hello, Debug!
The answer is 42
```
**주의:** `System.Diagnostics.Debug.WriteLine` 출력은 디버그 모드에서만 보입니다.

## Deep Dive (깊이 탐구)
디버그 출력은 개발 과정에서 오래전부터 사용돼 왔습니다. 조기 컴퓨터 시대, 램프와 전기 스위치가 상태를 보여줬죠. 그 후, 콘솔 로그가 그 자리를 차지했습니다.

대안으로 로거 라이브러리가 있습니다(`NLog`, `log4net`, `Serilog` 같은). 이들은 출력을 파일, 데이터베이스, 또는 다른 대상으로 설정할 수 있어요.

`Console.WriteLine`은 단순하지만, 멀티스레드 애플리케이션에서 동기화 문제를 일으킬 수 있습니다. `System.Diagnostics.Debug`와 `System.Diagnostics.Trace`는 개발하면서 조절할 수 있는 복잡한 출력을 제공합니다.

## See Also (추가 자료)
- Microsoft Docs on Debug Class: [System.Diagnostics.Debug Class](https://docs.microsoft.com/en-us/dotnet/api/system.diagnostics.debug)
- Introduction to logging in .NET: [Logging in .NET](https://docs.microsoft.com/en-us/dotnet/core/extensions/logging)
- Stack Overflow: When to use Console.WriteLine vs Debug.WriteLine vs Trace.WriteLine in .NET? [Console vs Debug vs Trace](https://stackoverflow.com/questions/4105120/when-to-use-console-writeline-vs-debug-writeline-vs-trace-writeline-in-net)
