---
title:    "C#: 표준 에러에 쓰는 방법"
keywords: ["C#"]
---

{{< edit_this_page >}}

## 왜

C# 프로그래밍에서 에러 메시지를 적절하게 처리하는 것은 중요한 요소입니다. 기본적으로 프로그램은 오류를 출력하고 사용자에게 이를 알려야합니다. 따라서 우리는 오류를 위한 적절한 출력 장치를 사용해야합니다. 이때 "standard error"는 가장 이상적인 선택입니다.

## 방법

C#에서 표준 출력을 사용하여 에러 메시지를 출력하는 방법을 쉽게 살펴보겠습니다. 먼저 코드 블록을 만들고 "Console.Error.WriteLine"을 사용하여 메시지를 출력하도록 코드를 작성하겠습니다. 

```C#
static void Main(string[] args)
{
    Console.Error.WriteLine("이것은 오류 메시지입니다.");
}
```

위의 코드를 실행하면 다음과 같은 출력이 나옵니다:

```
이것은 오류 메시지입니다.
```

이렇게 간단히 표준 에러로 오류를 보낼 수 있습니다. 그러나 실제로 프로그래밍에서는 더 복잡한 오류 처리가 필요할 수 있습니다.

## 깊이 들어가기

코드에서 메시지를 출력하는 것 뿐만 아니라, 표준 오류를 사용하면 특정한 종류의 메시지를 출력할 수도 있습니다. 예를 들어, "Console.Error.WriteLine" 대신 "Console.Error.Write"를 사용하면 같은 줄에 오류 메시지를 출력할 수 있습니다. 

```C#
static void Main(string[] args)
{
    Console.Error.Write("이것은 ");
    Console.Error.Write("오류 ");
    Console.Error.WriteLine("메시지입니다.");
}
```

위의 코드를 실행하면 다음과 같은 출력이 나옵니다:

```
이것은 오류 메시지입니다.
```

또 다른 예로는 표준 에러의 색을 변경하는 것입니다. 이를 위해서는 "Console.ForegroundColor" 메서드를 사용하여 메시지의 색을 변경하면 됩니다. 

```C#
static void Main(string[] args)
{
    Console.ForegroundColor = ConsoleColor.Red;
    Console.Error.WriteLine("이것은 빨간색으로 출력되는 에러 메시지입니다.");
}
```

위의 코드를 실행하면 다음과 같은 출력이 나옵니다:

```
이것은 빨간색으로 출력되는 에러 메시지입니다.
```

이처럼 표준 에러를 유용하게 사용할 수 있습니다. 그러나 프로그래밍에서는 여러 가지 다른 방식으로 표준 에러를 다룰 수도 있습니다. 디버깅에 더 많은 정보를 포함하거나, 다른 로그 파일에 저장하는 것도 가능합니다. 따라서 프로그래머는 자신에게 가장 적합한 방식을 선택할 수 있습니다.

## 참고하기

- [C# 표준 출력으로 에러 메시지 출력하기](https://zetawiki.com/wiki/C%23_%ED%91%9C%EC%A4%80_%EC%B6%9C%EB%A0%A5%EC%9C%BC%EB%A1%9C_%EC%97%90%EB%9F%AC_%EB%A9%94%EC%8B%9C%EC%A7%80_%EC%B6%9C%EB%A0%A5%ED%95%98%EA%B8%B0)
- [C# 디버깅을 위한 표준 에러 출력](https://docs.microsoft.com/ko-kr/dotnet/standard/io/how-to-write-to-and-read-from-the-console)
- [C# Console 클래스의 Error 속성](https://docs.microsoft.com/ko-kr/dotnet/api/system.console.error)