---
title:                "C#: 랜덤 숫자 생성"
programming_language: "C#"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜

우리가 가끔씩 우리의 프로그램에 무작위로 생성된 숫자를 필요로 할 때가 있습니다. 이럴 때는 어떤 방식으로 해당 숫자를 만들 수 있는지 알아보고 싶을 수 있습니다. 이 블로그 포스트에서는 C#에서 무작위 숫자를 생성하는 방법에 대해 알아보겠습니다.

## 어떻게

우선 우리는 Random 라이브러리를 사용하여 무작위 숫자를 생성할 수 있습니다. 다음 코드 블록을 사용하여 한가지 방법을 살펴보겠습니다.

```C#
Random random = new Random(); // Random 객체 생성
int randomNumber = random.Next(); // 다음 무작위 숫자 생성
Console.WriteLine(randomNumber); // 생성된 숫자 출력
```

위 코드를 실행하면 무작위로 생성된 숫자가 출력됩니다. 하지만 이 코드는 항상 같은 숫자를 출력할 수도 있습니다. 이러한 경우 시드 값을 설정하여 숫자를 더욱 무작위하게 만들 수 있습니다. 다음 코드 블록을 사용하여 시드 값을 설정하는 방법을 알아보겠습니다.

```C#
Random random = new Random(Guid.NewGuid().GetHashCode()); // 시드 값을 설정한 Random 객체 생성
int randomNumber = random.Next(); // 다음 무작위 숫자 생성
Console.WriteLine(randomNumber); // 생성된 숫자 출력
```

위 코드에서 우리는 GUID를 사용하여 시드 값을 설정하여 숫자를 더욱 무작위하게 만들었습니다. 이 외에도 다양한 방법으로 시드 값을 설정할 수 있으며, 이를 이용해 원하는 만큼 무작위성을 조절할 수 있습니다.

## 딥 다이브

C#에서 무작위 숫자를 생성할 수 있는 라이브러리는 Random을 비롯해 다양합니다. 하지만 이 라이브러리들은 사실 완전히 무작위한 숫자를 생성하지는 못합니다. 컴퓨터는 알고리즘을 사용하여 숫자를 생성하기 때문에, 어떤 알고리즘을 사용하느냐에 따라 숫자의 무작위성이 결정됩니다.

따라서 숫자의 무작위성을 높이기 위해서는 어떤 알고리즘을 사용하는지, 어떤 시드 값을 설정하는지 등에 대해 더욱 깊이 알아보는 것이 필요합니다. 또한 많은 수학적 이론과 알고리즘이 숨겨져 있기 때문에, 이를 공부하여 더 효율적이고 무작위한 숫자를 생성할 수 있도록 하는 것도 가능합니다.

## 관련 링크

[Random 클래스 - Microsoft Docs](https://docs.microsoft.com/ko-kr/dotnet/api/system.random?view=net-5.0)

[Random 클래스 예제 - C# 프로그래밍 스터디](https://www.csharpstudy.com/Classes/random.aspx)

[컴퓨터는 어떻게 난수를 생성할까요? - DailyEngineering](https://dailyengineering.kr/how-computers-generate-random-numbers/)