---
title:                "커맨드 라인 인자 읽기"
html_title:           "C#: 커맨드 라인 인자 읽기"
simple_title:         "커맨드 라인 인자 읽기"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 왜
 
명령 줄 인수를 읽는 방법을 배우는 것이 유용할 수 있습니다. 예를 들어, 사용자로부터 전달받은 정보에 따라 프로그램의 동작을 조정하거나 데이터를 처리할 수 있기 때문입니다.

## 어떻게

명령 줄 인수를 읽기 위해서는 `Main()` 메서드의 파라미터로 `string[] args`를 추가해야 합니다. 이 배열에는 사용자가 입력한 각각의 인수가 저장됩니다. 다음은 간단한 예제 코드와 그 실행 결과입니다.

```C#
using System;

class Program
{
    static void Main(string[] args)
    {
        Console.WriteLine("전달받은 인수의 개수: " + args.Length);
        for (int i = 0; i < args.Length; i++)
        {
            Console.WriteLine("인수 " + (i+1) + ": " + args[i]);
        }
    }
}
```

컴파일 후 프로그램을 실행할 때 인수를 함께 전달하면 다음과 같은 결과를 볼 수 있습니다.

```
dotnet Program.cs arg1 arg2 arg3
전달받은 인수의 개수: 3
인수 1: arg1
인수 2: arg2
인수 3: arg3
```

## 딥 다이브

만약 사용자가 잘못된 인수를 전달하거나 인수를 전혀 입력하지 않았을 경우, 예외 처리를 해주는 것이 중요합니다. 이를 위해 `args` 배열의 길이를 확인하고 필요한 예외 처리를 추가하는 것이 좋습니다. 또한, 프로그램 실행 전에 사용자에게 어떤 인수를 전달해야 하는지 설명하는 도움말을 출력하는 것도 도움이 될 수 있습니다.

# 참고 자료

- [C# 공식 문서 - Main 메서드 및 명령줄 인수](https://docs.microsoft.com/ko-kr/dotnet/csharp/programming-guide/main-and-command-args/)
- [C# 프로그래밍 책 - 섹션 8.7: 커맨드 라인 인수 처리](https://opentutorials.org/module/227/2671)