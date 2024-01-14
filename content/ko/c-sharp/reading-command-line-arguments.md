---
title:    "C#: 명령줄 인수 읽기"
keywords: ["C#"]
---

{{< edit_this_page >}}

# 왜: 커맨드 라인 인수를 읽는 것에 대해 읽으려는 이유

커맨드 라인 인수는 프로그램을 실행할 때 중요한 역할을 합니다. 이전에 입력했던 인수를 다시 사용하거나, 추가적인 정보를 입력하는 등 다양한 목적으로 사용할 수 있습니다. 따라서 프로그래밍을 할 때 커맨드 라인 인수를 제대로 읽는 것이 중요한데, 이를 위해 C#에서는 어떻게 읽을 수 있는지 알아보겠습니다.

## 어떻게: 커맨드 라인 인수를 읽는 방법

커맨드 라인 인수를 읽는 방법은 간단합니다. 우선, `Main` 메소드에 `string[] args` 매개변수를 추가합니다. 그리고 `args` 배열에는 프로그램이 실행될 때 입력된 인수들이 저장됩니다. 예를 들어, `program.exe input1 input2`와 같이 실행했다면 `args` 배열에는 `input1`과 `input2`가 저장됩니다.

이제 실제로 `args` 배열의 인수들을 읽어보겠습니다. 아래는 간단한 예제 코드입니다.

```C#
using System;

class Program
{
    public static void Main(string[] args)
    {
        // 입력된 인수들을 출력하기
        for(int i = 0; i < args.Length; i++)
        {
            Console.WriteLine(args[i]);
        }
    }
}
```

위 코드를 실행하면 `input1`과 `input2`가 순서대로 출력되는 것을 확인할 수 있습니다. 이처럼 `args` 배열의 각 인덱스에는 입력된 인수들이 저장되므로, 원하는 방식으로 활용할 수 있습니다.

## 더 깊이 파고들기: 커맨드 라인 인수 읽기에 대한 추가 정보

커맨드 라인 인수를 읽는 방법 외에도, `Environment.GetCommandLineArgs` 메소드를 사용해 인수를 읽을 수도 있습니다. 이 메소드는 `Program` 클래스의 `Main` 메소드에서만 호출할 수 있고, `args` 배열과 동일하게 입력된 인수들을 리턴합니다.

또한 커맨드 라인 인수에는 유용한 옵션들도 있습니다. 예를 들어, `program.exe -h`와 같이 실행하면 `h`라는 옵션이 적용되지만 인수로 인식되지 않습니다. 이처럼 인수 외에도 옵션을 사용하고 싶다면, `args` 배열을 분석하여 처리할 수 있습니다.

# 더 보기

- [C# 메소드](https://www.c-sharpcorner.com/blogs/understanding-method-in-c-sharp)
- [C# 명령줄 인수 읽기](https://www.tutorialsteacher.com/csharp/command-line-argument-csharp)