---
date: 2024-01-27 20:32:47.631238-07:00
description: "\uBC29\uBC95: C#\uC5D0\uC11C \uB09C\uC218\uB97C \uC0DD\uC131\uD558\uB294\
  \ \uAC00\uC7A5 \uC77C\uBC18\uC801\uC778 \uBC29\uBC95\uC740 `System.Random` \uD074\
  \uB798\uC2A4\uB97C \uC0AC\uC6A9\uD558\uB294 \uAC83\uC785\uB2C8\uB2E4. \uB2E4\uC74C\
  \uC740 \uADF8 \uC0AC\uC6A9\uBC95\uC744 \uBCF4\uC5EC\uC8FC\uB294 \uAC04\uB2E8\uD55C\
  \ \uC608\uC81C\uC785\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.227814-06:00'
model: gpt-4-0125-preview
summary: "C#\uC5D0\uC11C \uB09C\uC218\uB97C \uC0DD\uC131\uD558\uB294 \uAC00\uC7A5\
  \ \uC77C\uBC18\uC801\uC778 \uBC29\uBC95\uC740 `System.Random` \uD074\uB798\uC2A4\
  \uB97C \uC0AC\uC6A9\uD558\uB294 \uAC83\uC785\uB2C8\uB2E4."
title: "\uB09C\uC218 \uC0DD\uC131"
weight: 12
---

## 방법:
C#에서 난수를 생성하는 가장 일반적인 방법은 `System.Random` 클래스를 사용하는 것입니다. 다음은 그 사용법을 보여주는 간단한 예제입니다:

```C#
using System;

public class RandomNumberExample
{
    static void Main(string[] args)
    {
        Random random = new Random();
        int randomNumber = random.Next(1, 100); // 1과 99 사이의 숫자를 생성합니다
        Console.WriteLine($"Random number: {randomNumber}");
    }
}
```

이것은 다음과 같은 난수를 출력합니다:

```
Random number: 42
```

0.0과 1.0 사이의 랜덤 부동 소수점 수를 생성하려면 `NextDouble` 메서드를 사용할 수 있습니다:

```C#
double randomDouble = random.NextDouble();
Console.WriteLine($"Random double: {randomDouble}");
```

암호학적인 무작위성이 필요한 보안에 민감한 애플리케이션에서 작업하는 경우, `System.Security.Cryptography`에 있는 `RNGCryptoServiceProvider` 클래스를 사용하는 것이 더 좋습니다:

```C#
using System;
using System.Security.Cryptography;

public class SecureRandomExample
{
    static void Main()
    {
        byte[] randomNumber = new byte[4]; // 4 바이트 길이의 난수를 생성합니다
        using (RNGCryptoServiceProvider rng = new RNGCryptoServiceProvider())
        {
            rng.GetBytes(randomNumber);
        }
        int value = BitConverter.ToInt32(randomNumber, 0);
        Console.WriteLine($"암호학적으로 안전한 난수: {value}");
    }
}
```

## 깊이 있게 알아보기
C#에서 난수 생성은 시간이 지남에 따라 발전해 왔습니다. 초기에는 `System.Random` 클래스가 유사 난수를 생성하기 위한 주요 선택이었습니다. 특정 시드 값이 주어지면 동일한 숫자 시퀀스를 생성하기 때문에 유사 난수라고 합니다. 이는 디버깅이나 테스트의 반복성에 유용할 수 있습니다.

기본적인 요구 사항에는 충분하지만 `System.Random`은 스레드 안전하지 않으며 예측 가능한 결과를 생성할 수 있어 보안 의존 애플리케이션에는 적합하지 않습니다. 이러한 제한은 보안이 더 강화된 암호학적 무작위성을 위해 `RNGCryptoServiceProvider`의 도입으로 이어졌습니다. 이는 더 안전하지만 리소스를 더 많이 사용합니다.

.NET Core 및 .NET 5+에서는 보안이 강화된 난수를 생성하기 위해 `System.Security.Cryptography`의 `RandomNumberGenerator` 클래스를 사용할 수 있는데, 이는 `RNGCryptoServiceProvider`에 비해 더 현대적이고 사용하기 쉬운 옵션으로 의도되었습니다.

C#에서 난수를 생성하는 각 방법은 애플리케이션의 요구 사항에 따라 그 자리를 차지합니다. 대부분의 애플리케이션의 경우 `System.Random`으로 충분하지만, 보안적으로 예측 불가능한 난수가 필요한 경우 암호학 클래스는 견고한 대안을 제공합니다.
