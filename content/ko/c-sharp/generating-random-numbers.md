---
title:                "랜덤 숫자 생성하기"
html_title:           "Rust: 랜덤 숫자 생성하기"
simple_title:         "랜덤 숫자 생성하기"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 무엇이고 왜하세요?

랜덤 숫자 생성은 예측 불가능한 숫자를 프로그램에서 만드는 것입니다. 프로그래머들은 게임, 시뮬레이션, 테스트 데이터 등에서 유용한 무작위성을 제공하기 위해 이를 사용합니다.

## 어떻게:

```C#
using System;

class Program
{
    static void Main()
    {
        Random random = new Random();
        int randomNumber = random.Next(1, 101);
        Console.WriteLine("생성된 랜덤 숫자는 {0}입니다", randomNumber);
    }
}
```
이 코드를 실행하면 1과 100 사이의 랜덤 숫자 하나를 출력합니다.

## 깊은 탐구: 

랜덤 숫자 생성은 1950년대부터 컴퓨터 과학에 적극적으로 사용되었습니다. 

C#에서 `Random` 클래스 이외에도 보다 암호학적으로 안전한 `RNGCryptoServiceProvider` 클래스를 사용하여 랜덤 숫자를 생성할 수 있습니다. 

`Random` 클래스에서 랜덤 숫자는 정해진 시드 값에 기반하여 생성되므로 완전히 랜덤하게 생성되는 것은 아닙니다. 그렇기 때문에 보안에 민감한 목적으로는 `RNGCryptoServiceProvider` 클래스를 사용하는 것이 더 적절합니다. 

## 참고 자료:

[System.Random (공식 Microsoft 문서)](https://docs.microsoft.com/en-us/dotnet/api/system.random?view=net-5.0)

[System.Security.Cryptography.RNGCryptoServiceProvider (공식 Microsoft 문서)](https://docs.microsoft.com/en-us/dotnet/api/system.security.cryptography.rngcryptoserviceprovider?view=net-5.0)