---
title:    "C#: 컴퓨터 프로그래밍에서의 커맨드 라인 인수 읽기"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

커맨드 라인 인수를 읽는 것은 C# 프로그래머에게 매우 중요합니다. 이 기술은 프로그램이 외부로부터 입력을 받아들일 수 있게 하여 상호작용성을 제공하고, 다양한 실행 옵션을 적용할 수 있게 해줍니다. 따라서 더 유연하고 다양한 사용자의 요구를 충족하는 프로그램을 만들 수 있습니다. 이러한 이유때문에 커맨드 라인 인수를 읽는 방법을 배우는 것은 모든 C# 프로그래머에게 도움이 될 것입니다.

## 어떻게

커맨드 라인 인수를 읽기 위해 아래와 같은 코드를 사용할 수 있습니다.

```C#
using System;

namespace CommandLineArguments
{
    class Program
    {
        static void Main(string[] args)
        {
            foreach(string arg in args)
            {
                Console.WriteLine(arg);
            }
        }
    }
}
```

위 예제는 `args` 배열의 각 인자를 `foreach`문을 사용해 하나씩 출력합니다. 이를 실행해보면, 프로그램을 실행할 때 커맨드 라인에 입력한 인수들이 모두 출력된 것을 볼 수 있습니다.

```
CommandLineArguments.exe 인수1 인수2
```

```
인수1
인수2
```

매우 간단한 예제이지만, 이를 바탕으로 더 복잡한 동작을 구현할 수 있습니다. 또한 `args` 배열의 각 인자에 대해 조건문을 사용해서 원하는 동작을 수행할 수도 있습니다. 예를 들어, 다음과 같이 `-i` 옵션을 사용하면 입력받은 문자열을 대문자로 변환하는 프로그램을 만들 수 있습니다.

```C#
using System;

namespace CommandLineArguments
{
    class Program
    {
        static void Main(string[] args)
        {
            foreach(string arg in args)
            {
                if(arg == "-i")
                {
                    Console.WriteLine("대문자로 출력합니다.");
                }
                else
                {
                    Console.WriteLine(arg.ToUpper());
                }
            }
        }
    }
}
```

이제 다음과 같이 실행해보면, `-i` 옵션을 사용할 때는 문자열이 대문자로 출력되고, 다른 인수를 입력할 때는 해당 문자열이 대문자로 변환되어 출력됩니다.

```
CommandLineArguments.exe -i hello world
```

```
대문자로 출력합니다.
HELLO
WORLD
```

이렇게 커맨드 라인 인수를 읽는 것은 프로그램에 더 많은 유용한 기능을 추가할 수 있게 해줍니다.

## 딥 다이브

`Main` 메소드에서 사용한 `args` 배열 외에도, `Environment.GetCommandLineArgs()` 메소드를 사용하여 커맨드 라인 인수를 가져올 수 있습니다. 이 메소드는 `string[]` 타입의 결과를 반환하며, 첫 번째 인수는 프로그램의 경로를 나타냅니다.

또한 커맨드 라인 인수를 처리하기 전에 유효성을 검사하는 것이 중요합니다. 예를 들어, 사용자가 정해진 형식과 다른 형식의 입력을 넣었을 때 문제가 발생할 수 있으며, 이를 해결해주는 코드를 작성해야 합니다.

## 참고

- [C# 공식 문서: 명령줄 인수](https://docs.microsoft.com/ko-kr/dotnet/csharp/programming-guide/main-and-command-args/command-line-arguments)
- [C# 커맨드 라인 인수 처리 방법](https://www.c-sharpcorner.com/uploadfile/mahesh/command-line-