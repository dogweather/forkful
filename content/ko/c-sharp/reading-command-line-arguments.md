---
title:                "커맨드 라인 인수 읽기"
html_title:           "C#: 커맨드 라인 인수 읽기"
simple_title:         "커맨드 라인 인수 읽기"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

명령줄 인수를 읽는 것이 무엇인지 알아보고 여러분이 그렇게 하는 이유를 알아보겠습니다. 명령줄 인수를 읽는다는 것은 프로그램이 실행될 때 전달된 인수를 읽어오는 것을 말합니다. 이를테면, 사용자가 프로그램을 실행할 때 인수로 전달한 값을 읽어와서 해당 값에 따라 프로그램이 실행되는 방식을 말합니다. 이로 인해 프로그래머는 프로그램의 동작을 조정할 수 있게 됩니다.

## 방법:

우리가 어떻게 명령줄 인수를 읽고 사용할 수 있는지 알아보겠습니다. 아래의 코드 블럭을 통해 간단한 예제를 살펴보고 결과도 확인해보세요.

```C#
using System;

class Program
{
  static void Main (String[] args)
  {
    Console.WriteLine("전달된 인수의 개수: " + args.Length);

    Console.WriteLine("전달된 인수: ");
    for(int i=0; i<args.Length; i++)
    {
      Console.WriteLine(args[i]);
    }
  }
}
```

```shell
$ dotnet run hello world
전달된 인수의 개수: 2
전달된 인수:
hello
world
```

위의 예제에서는 사용자가 프로그램을 실행할 때 "hello"와 "world"라는 두 개의 인수를 전달했습니다. 그리고 우리는 이 인수들을 읽어와서 화면에 출력하고 있습니다.

## 깊게 파헤치기:

명령줄 인수를 읽는 것은 우리가 현재 사용하고 있는 컴퓨터 시스템의 예전부터 사용되어 온 기술입니다. 예전에는 컴퓨터가 프로그램을 실행할 때 사용자가 전달한 명령을 그대로 사용하였기 때문에 명령줄 인수를 읽는 것이 중요했습니다. 하지만 지금은 그렇지 않습니다. 대부분의 프로그램에서는 GUI(Graphical User Interface)를 사용하고 있기 때문에 명령줄 인수를 읽기보다는 다른 방식으로 사용자와 상호작용합니다.

하지만 명령줄 인수를 사용하는 것에는 여전히 장점이 있습니다. 사용자가 CLI(Command-line interface) 프로그램을 사용할 때 유용합니다. 또한 배치 스크립트에서 프로그램을 실행할 때도 매우 유용합니다.

명령줄 인수를 읽는 방식은 프로그래밍 언어마다 조금씩 다를 수 있지만, 기본적인 개념은 비슷합니다. 우리는 명령줄 인수를 배열 형태로 받아서 처리할 수 있고, 사용자가 전달한 인수에 따라 프로그램의 동작을 조정할 수 있습니다.

## 관련 자료:

- [CommandLine Class (System.CommandLine)](https://docs.microsoft.com/en-us/dotnet/api/system.commandline.commandline?view=dotnet-plat-ext-5.0)
- [Command-Line Arguments (Microsoft C# Programming Guide)](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/main-and-command-args/command-line-arguments)