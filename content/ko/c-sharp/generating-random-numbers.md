---
title:                "난수 생성"
html_title:           "C#: 난수 생성"
simple_title:         "난수 생성"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

# 무엇 & 왜?

난수 생성은 무작위로 생성된 숫자들의 시퀀스입니다. 프로그래머들은 보안, 게임, 데이터 분석 등 다양한 분야에서 난수를 사용합니다.

# 방법:

```c#
// 1부터 10까지 난수 생성
Random random = new Random();
int randomNumber = random.Next(1, 11);
Console.WriteLine(randomNumber);
// 출력: 7
```

```c#
// 주어진 배열에서 난수 생성
int[] numbers = { 1, 2, 3, 4, 5 };
Random random = new Random();
int randomIndex = random.Next(0, numbers.Length);
Console.WriteLine($"Random number from array: {numbers[randomIndex]}");
// 출력: Random number from array: 3
```

# 심층 분석:

난수 생성에 대한 역사적 배경은 굉장히 흥미롭습니다. 초기에는 컴퓨터들이 무작위로 숫자들을 생성하기 위해 주사위를 사용했지만, 현재는 알고리즘을 사용하여 난수를 생성합니다. 다른 프로그래밍 언어에서도 난수를 생성하는 기능을 제공하지만, C#에서는 System.Random 클래스를 사용하여 더 쉽게 난수를 생성할 수 있습니다. 또한, 사용자가 지정한 시작값과 끝값 사이에서 난수를 생성할 수 있습니다.

# 관련 자료:

- https://docs.microsoft.com/en-us/dotnet/api/system.random?view=netcore-3.1: 난수 생성을 위한 Microsoft 공식 문서
- https://www.tutorialspoint.com/csharp/csharp_random_numbers.htm: C#에서 난수 생성하는 방법에 대한 튜토리얼
- https://stackoverflow.com/questions/2706500/why-do-we-need-a-random-class-object-in-c: C#에서 랜덤 클래스 객체를 사용해야 하는 이유에 대한 스택 오버플로우 질문 및 답변