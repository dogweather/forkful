---
title:                "명령줄 인수 읽기"
date:                  2024-01-20T17:55:58.925527-07:00
model:                 gpt-4-1106-preview
simple_title:         "명령줄 인수 읽기"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가요? 왜 사용하는 건가요?)
커맨드 라인 인자 읽기는 사용자가 프로그램을 시작할 때 파라미터를 전달하는 방법입니다. 이를 통해 동적으로 프로그램의 행동을 조정할 수 있기 때문에 개발자들이 사용합니다.

## How to: (어떻게 사용하나요?)
```C#
using System;

class Program
{
    static void Main(string[] args)
    {
        Console.WriteLine("안녕하세요! 커맨드 라인 인자를 읽어봅시다.");

        for (int i = 0; i < args.Length; i++)
        {
            Console.WriteLine($"인자 {i}: {args[i]}");
        }
    }
}
```

콘솔에 실행할 때 인자를 다음과 같이 넣을 수 있습니다:

```
dotnet run -- 사과 바나나 체리
```

출력 예시:
```
안녕하세요! 커맨드 라인 인자를 읽어봅시다.
인자 0: 사과
인자 1: 바나나
인자 2: 체리
```

## Deep Dive (심층 분석)
커맨드 라인 인자를 읽는 것은 오래전부터 사용되었습니다. 유닉스 시스템에서 시작해 지금의 여러 운영 체제와 언어에서 널리 쓰입니다.

대안으로는 환경 변수, 구성 파일, 대화형 입력 등이 있습니다만, 커맨드 라인 인자는 직접적이고 간단한 방법으로 남아 있습니다.

C#에서는 `string[] args`를 `Main` 메서드의 파라미터로 사용해 인자를 받습니다. `args.Length`로 개수를 확인하고, 인덱스로 각 인자를 사용할 수 있습니다.

## See Also (참고 자료)
- [Microsoft Docs - Main() and command-line arguments](https://docs.microsoft.com/dotnet/csharp/programming-guide/main-and-command-args/)
- [Microsoft Docs - Command-Line Arguments (C# Programming Guide)](https://docs.microsoft.com/dotnet/csharp/programming-guide/main-and-command-args/command-line-arguments)
- [Stack Overflow - How can I read command line parameters from an application?](https://stackoverflow.com/questions/491595/best-way-to-parse-command-line-arguments-in-c)
