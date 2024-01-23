---
title:                "표준 오류로 쓰기"
html_title:           "Bash: 표준 오류로 쓰기"
simple_title:         "표준 오류로 쓰기"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
표준 오류로 쓰기는 프로그램의 오류 메시지나 진단 정보를 출력하는 방법입니다. 개발자들은 사용자에게 유용한 오류 정보를 제공하고 로깅 시스템에서 오류를 분리하기 위해서 이를 사용합니다.

## How to: (방법)
```C#
using System;

class StdErrExample
{
    static void Main()
    {
        // 오류 메시지를 콘솔의 표준 오류 스트림으로 출력합니다.
        Console.Error.WriteLine("오류 발생: 잘못된 입력입니다.");

        // 표준 오류를 사용하는 다른 방법
        var errorWriter = Console.Error;
        errorWriter.WriteLine("오류 발생: 파일을 찾을 수 없습니다.");
    }
}
```

실행 결과는 커맨드라인에 다음과 같이 표시됩니다:

```
오류 발생: 잘못된 입력입니다.
오류 발생: 파일을 찾을 수 없습니다.
```

## Deep Dive (심화 학습)
표준 오류는 유닉스 시스템의 초기부터 존재했습니다. `stderr`는 프로세스가 시작될 때 생기며, 일반적으로 콘솔이나 터미널에 연결됩니다. 표준 출력(`stdout`)과는 다르게, 로깅이나 오류 메시지에 사용됩니다. C#에서는 `Console.Error`를 통해 이 스트림에 접근할 수 있고, 이것은 `TextWriter` 타입입니다. 리디렉션과 파이프라인을 통해 쉽게 오류 메시지를 다른 파일이나 도구로 보낼 수 있습니다.

## See Also (관련 자료)
- Microsoft Docs – Console.Error 속성: [Console.Error Property](https://docs.microsoft.com/en-us/dotnet/api/system.console.error)
- Microsoft Docs – TextWriter 클래스: [TextWriter Class](https://docs.microsoft.com/en-us/dotnet/api/system.io.textwriter)
- Stack Overflow – 표준 오류: 언제 어떻게 사용할까요?: [When to use standard error stream in C#?](https://stackoverflow.com/questions/3811464/when-to-use-standard-error-stream-in-c)
