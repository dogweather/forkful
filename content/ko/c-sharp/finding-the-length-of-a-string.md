---
title:    "C#: 문자열의 길이를 찾는 방법"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# 왜

문자열의 길이를 찾는 것이 왜 중요한지 궁금하셨나요? 문자열의 길이를 찾는 것은 컴퓨터 프로그래밍에서 매우 일반적인 작업입니다. 예를 들어, 사용자가 글자 수 제한이 있는 채팅 메시지를 작성하거나 비밀번호의 최소/최대 길이를 설정할 때 문자열의 길이를 확인합니다.

# 어떻게

문자열의 길이를 찾는 것은 C#을 사용하여 간단합니다. 아래는 "Hello, world!"라는 문자열의 길이를 찾는 예제 코드입니다.

```C#
string str = "Hello, world!";
int length = str.Length;
Console.WriteLine("문자열의 길이는 " + length + "입니다.");
```

출력은 다음과 같습니다.

```
문자열의 길이는 13입니다.
```

다른 예제를 살펴보면, 사용자로부터 입력받은 문자열의 길이를 찾는 것도 간단합니다. 아래는 사용자가 입력한 문자열의 길이를 찾는 예제 코드입니다.

```C#
Console.Write("문자열을 입력하세요: ");
string str = Console.ReadLine();
int length = str.Length;
Console.WriteLine("입력한 문자열의 길이는 " + length + "입니다.");
```

출력은 다음과 같습니다.

```
문자열을 입력하세요: Hello, world!
입력한 문자열의 길이는 13입니다.
```

# 깊이 파고들기

문자열의 길이를 찾는 것은 어떻게 동작하는지 궁금하신가요? 문자열의 길이를 찾는 방법은 내부적으로 매우 간단합니다. C#에서 제공하는 `Length` 속성을 사용하여 문자열의 길이를 나타내는 정수 값을 얻습니다. 이 값은 문자열의 실제 길이가 아니라 문자열에서 사용된 char의 수를 나타냅니다. 또한, 이 값은 32비트 정수로 표현됩니다.

# 참고

- C# 문자열의 길이 확인하기: https://docs.microsoft.com/ko-kr/dotnet/api/system.string.length
- C# 콘솔 애플리케이션 만들기: https://docs.microsoft.com/ko-kr/dotnet/core/tutorials/with-visual-studio-code