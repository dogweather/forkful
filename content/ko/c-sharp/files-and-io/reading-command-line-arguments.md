---
date: 2024-01-20 17:55:58.925527-07:00
description: "\uCEE4\uB9E8\uB4DC \uB77C\uC778 \uC778\uC790 \uC77D\uAE30\uB294 \uC0AC\
  \uC6A9\uC790\uAC00 \uD504\uB85C\uADF8\uB7A8\uC744 \uC2DC\uC791\uD560 \uB54C \uD30C\
  \uB77C\uBBF8\uD130\uB97C \uC804\uB2EC\uD558\uB294 \uBC29\uBC95\uC785\uB2C8\uB2E4\
  . \uC774\uB97C \uD1B5\uD574 \uB3D9\uC801\uC73C\uB85C \uD504\uB85C\uADF8\uB7A8\uC758\
  \ \uD589\uB3D9\uC744 \uC870\uC815\uD560 \uC218 \uC788\uAE30 \uB54C\uBB38\uC5D0 \uAC1C\
  \uBC1C\uC790\uB4E4\uC774 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.257362-06:00'
model: gpt-4-1106-preview
summary: "\uCEE4\uB9E8\uB4DC \uB77C\uC778 \uC778\uC790 \uC77D\uAE30\uB294 \uC0AC\uC6A9\
  \uC790\uAC00 \uD504\uB85C\uADF8\uB7A8\uC744 \uC2DC\uC791\uD560 \uB54C \uD30C\uB77C\
  \uBBF8\uD130\uB97C \uC804\uB2EC\uD558\uB294 \uBC29\uBC95\uC785\uB2C8\uB2E4. \uC774\
  \uB97C \uD1B5\uD574 \uB3D9\uC801\uC73C\uB85C \uD504\uB85C\uADF8\uB7A8\uC758 \uD589\
  \uB3D9\uC744 \uC870\uC815\uD560 \uC218 \uC788\uAE30 \uB54C\uBB38\uC5D0 \uAC1C\uBC1C\
  \uC790\uB4E4\uC774 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uBA85\uB839\uC904 \uC778\uC218 \uC77D\uAE30"
weight: 23
---

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
