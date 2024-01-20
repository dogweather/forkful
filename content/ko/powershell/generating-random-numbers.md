---
title:                "임의의 숫자 생성하기"
html_title:           "Elixir: 임의의 숫자 생성하기"
simple_title:         "임의의 숫자 생성하기"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하는가?

난수 생성이란 프로그램 실행시마다 변경되는 랜덤한 값을 의미합니다. 프로그래머들은 종종 유니크한 ID 생성, 테스트 데이터 생성등에 이를 활용합니다.

## 어떻게 사용하는가:

PowerShell에서 난수를 생성하는 방법은 아래의 코드 예제와 같습니다.

```PowerShell
# Get-Random cmdlet 을 사용한 예제
$randomNumber = Get-Random -Minimum 0 -Maximum 100
Write-Host "생성된 난수는 $randomNumber 입니다."
```

이 코드를 실행하면 0에서 100 사이의 무작위 수가 생성됩니다.
 
## 디프다이브:

난수를 생성하는 메서드는 오래 전부터 존재했습니다. 가장 초기의 방법 중 하나는 물리적인 개입을 통한 방법이었습니다 (예: 주사위 던지기). 

하지만 컴퓨터 시대에 들어서며, 효율적이고 반복 가능한 난수 생성 방법이 필요해졌습니다. 그래서 파워쉘 같은 프로그래밍 언어에서는 난수 생성 메서드를 기본적으로 지원하고 있습니다.

파워쉘에서는 `Get-Random` cmdlet을 통해 간편하게 난수를 생성할 수 있습니다. 내부적으로는 .NET의 System.Random 클래스를 사용하여 구현되어 있습니다.

대체 방법으로는 난수 생성 알고리즘에 해당하는 Mersenne Twister 같은 라이브러리를 직접 사용하는 것도 있습니다.

## 참고자료:

- Get-Random에 대한 공식 문서: [https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-random](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-random)
- System.Random 클래스 정보: [https://docs.microsoft.com/en-us/dotnet/api/system.random](https://docs.microsoft.com/en-us/dotnet/api/system.random)
- Mersenne Twister 알고리즘 추가 정보: [https://en.wikipedia.org/wiki/Mersenne_Twister](https://en.wikipedia.org/wiki/Mersenne_Twister)