---
title:    "C#: 랜덤 숫자 생성하기"
keywords: ["C#"]
---

{{< edit_this_page >}}

# 왜 랜덤 숫자를 생성하는가?

랜덤 숫자를 생성하는 것은 프로그래밍에서 매우 중요합니다. 랜덤 숫자를 사용하면 다양한 기능을 구현할 수 있습니다. 예를 들어, 게임에서 더욱 다양한 경험을 제공하거나, 데이터를 분석할 때 표본을 뽑는 등 다양한 영역에서 랜덤 숫자는 매우 유용합니다. 그러므로 프로그래밍을 하는데 있어 랜덤 숫자를 생성하는 것은 필수적입니다.

## 어떻게 생성할까요?

C#에서 랜덤 숫자를 생성하는 방법은 매우 간단합니다. 먼저, `Random` 클래스를 사용하여 객체를 생성한 다음 `Next()` 메소드를 호출하면 됩니다. 이 메소드는 0부터 지정한 숫자까지의 랜덤한 숫자를 반환합니다.

```C#
Random rand = new Random();
int num = rand.Next(10); // 0부터 9까지의 랜덤한 숫자
```

또한 `Next()` 메소드를 통해 범위를 지정할 수도 있습니다. 예를 들어, `Next(5, 10)`은 5부터 9까지의 랜덤한 숫자를 반환합니다.

```C#
int numRange = rand.Next(5, 10); // 5부터 9까지의 랜덤한 숫자
```

## 딥 다이브

C#에서는 `Random` 클래스를 이용하여 더욱 다양한 랜덤 숫자를 생성할 수 있습니다. 예를 들어, `NextDouble()` 메소드는 0부터 1 사이의 랜덤한 소수를 반환합니다. 또한 `NextBytes()` 메소드를 사용하여 바이트 배열을 채울 수도 있습니다.

또한 이를 활용하여 랜덤한 문자열을 생성할 수도 있습니다. 예를 들어, 알파벳 소문자와 숫자를 조합하여 8자리의 랜덤한 문자열을 생성하는 방법은 다음과 같습니다.

```C#
string chars = "abcdefghijklmnopqrstuvwxyz0123456789";
string randomString = new string(Enumerable.Repeat(chars, 8)
  .Select(s => s[rand.Next(s.Length)]).ToArray());
Console.WriteLine(randomString); // 랜덤한 8자리 문자열 출력
```

이처럼 C#에서 랜덤 숫자를 생성하는 방법은 다양하며, 더욱 깊이 공부한다면 더 흥미로운 방법을 알게 될 수 있습니다.

# 관련 링크

- https://docs.microsoft.com/ko-kr/dotnet/api/system.random?view=net-5.0
- https://www.c-sharpcorner.com/blogs/random-number-generation-in-c-sharp-programming1
- https://www.tutorialspoint.com/csharp/csharp_random_numbers.htm
- https://www.c-sharpcorner.com/submission/1b76dff8-fc35-4c1c-99c2-332cdf418626
- https://www.codeproject.com/Articles/25285/How-to-generate-a-random-number-in-a-specific-rang