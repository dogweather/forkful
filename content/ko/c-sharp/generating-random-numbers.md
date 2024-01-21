---
title:                "난수 생성하기"
date:                  2024-01-20T17:48:37.749129-07:00
model:                 gpt-4-1106-preview
simple_title:         "난수 생성하기"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
랜덤 숫자를 생성하는 것은 예측 불가능한 숫자를 만드는 과정입니다. 프로그래머들은 게임, 시뮬레이션, 보안 등 다양한 분야에서 예상치 못한 결과를 필요로 할 때 이를 사용합니다.

## How to: (방법)
```C#
using System;

class Program
{
    static void Main()
    {
        Random random = new Random();
        int randomNumber = random.Next(1, 100);  // 1부터 99까지의 랜덤 숫자 생성
        Console.WriteLine(randomNumber);    
    }
}
```
샘플 출력:
```
42
```

## Deep Dive (심층 분석)
C#에서 `System.Random` 클래스는 의사랜덤수(pseudorandom number)를 생성합니다. "의사"란 실제로는 패턴이 있지만 충분히 불규칙적으로 보인다는 것을 의미합니다. .NET 초기 버전부터 이 클래스를 사용해 왔고 현재까지도 널리 사용되고 있습니다.

대안으로, 보안에 더 중요한 곳에서는 `System.Security.Cryptography.RandomNumberGenerator` 클래스를 사용할 수 있습니다. 일반적인 랜덤 클래스보다 예측하기 훨씬 어렵습니다.

더 깊게 들어가면, `Random` 클래스는 시드 값에 기반하여 숫자를 생성합니다. 같은 시드 값을 사용하면 동일한 숫자 시퀀스가 나타납니다. 이는 유닛 테스트나 디버깅에 유용할 수 있습니다.

## See Also (참고 자료)
- [Microsoft Random Class Documentation](https://docs.microsoft.com/en-us/dotnet/api/system.random?view=net-6.0)
- [Microsoft RandomNumberGenerator Class Documentation](https://docs.microsoft.com/en-us/dotnet/api/system.security.cryptography.randomnumbergenerator?view=net-6.0)
- [.NET API Browser](https://docs.microsoft.com/en-us/dotnet/api/)