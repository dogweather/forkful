---
title:                "명령줄 인수 읽기"
html_title:           "Arduino: 명령줄 인수 읽기"
simple_title:         "명령줄 인수 읽기"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜 & 왜?

명령줄 인자 읽기는 프로그램이 실행될 때 받은 인자를 읽는 일련의 프로세스입니다. 이는 사용자가 프로그램의 행동을 조절하거나, 자동화하거나, 타스크를 실행하는 데 필요한 정보를 제공할 수 있게 합니다.

## 해 보기:

다음은 간단하게 C#에서 명령행 인자를 읽는 예시입니다:

```C#
public class Program
{
    public static void Main(string[] args)
    {
        for(int i = 0; i<args.Length; i++)
        {
            Console.WriteLine($"Argument {i} is {args[i]}.");
        }
    }
}
```

이 코드가 실행되고 "apple", "banana", "cherry"라는 인자들을 받으면 다음과 같은 출력을 내놓음:

```
Argument 0 is apple.
Argument 1 is banana.
Argument 2 is cherry.
```

## 깊이 있게 살펴보기:

### 역사적 맥락
명령줄 인자는 리눅스 및 UNIX 쉘 스크립트에서 오래 전부터 널리 사용되어 왔습니다. 이 방법은 사용자가 명령어를 입력하면서 애플리케이션이 어떻게 동작해야 하는지 지정할 수 있게 해줍니다.

### 대체 방법
명령줄 인자 외에도 사용자가 프로그램에 데이터를 제공하는 방법은 여러가지 있습니다: 환경 변수, 파일 읽기, 표준 입력, GUI 입력 필드 등이 있습니다.

### 구현 세부 사항
C#에서 `args` 배열은 명령줄 인자들을 담아두는데 사용됩니다. 이 때, 처음 요소(즉, `args[0]`)은 프로그램의 이름이 아닌 첫 번째 인자를 가리킵니다. 이는 일부 다른 언어들(예: C, Python)과는 다른 점입니다.

## 참고할 만한 링크:

- [Microsoft Docs: Command-Line Arguments (C# Programming Guide)](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/main-and-command-args)
- [Stackoverflow: How do I use command line arguments in C#?](https://stackoverflow.com/questions/8141/how-do-i-use-command-line-arguments-in-csharp)