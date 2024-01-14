---
title:    "C#: 미래나 과거의 날짜 계산하기"
keywords: ["C#"]
---

{{< edit_this_page >}}

## 왜

날짜를 미래나 과거로 계산하는 것에 참여해야할 이유는 무엇일까요? 우리는 인생에서 많은 날짜를 기억하고 기념하기 위해서이기도 하지만, 때로는 특정한 날짜를 계산해야하는 경우도 있을 겁니다. 예를 들어, 한국의 법정 공휴일을 알아보기 위해서는 어떤 날짜가 일요일인지 체크해야할 수도 있습니다.

## 어떻게

이제 C#을 사용해서 미래나 과거 날짜를 계산하는 방법을 알아보도록 하겠습니다. 아래는 코드 블록 안에 있는 ```C#``` 예제와 샘플 출력입니다.

```C#
// 현재 날짜부터 100일 후의 날짜 계산
DateTime currentDate = DateTime.Today;
DateTime futureDate = currentDate.AddDays(100);
Console.WriteLine(futureDate.ToString("D"));

// 현재 날짜부터 2주 전의 날짜 계산
DateTime pastDate = currentDate.AddDays(-14);
Console.WriteLine(pastDate.ToString("D"));
```
다음은 위 코드의 출력 결과입니다.
```
7월 27일 2021년
7월 13일 2021년
```

이와 같은 방법을 사용하여 원하는 날짜를 계산할 수 있습니다. 또한 C#에서는 날짜를 원하는 형식으로 출력하는 다양한 방법이 있으니 원하는 형식에 맞게 설정하여 사용하시면 됩니다.

## 깊게 파고들기

날짜를 계산하는 것은 간단한 것처럼 보일 수 있습니다. 하지만 실제로는 많은 깊은 지식과 로직이 필요합니다. 예를 들어, 윤년이나 다양한 국가의 공휴일을 고려해야 할 때도 있습니다. 또한, 날짜를 계산하는 데에도 다양한 방식이 있기 때문에 어떤 방법이 가장 효율적인지 고민해야 할 수도 있습니다.

프로그래밍에서 날짜와 시간은 중요한 요소 중 하나이기 때문에, 추가적인 학습 및 연습이 필요합니다. C# 외에도 다른 프로그래밍 언어에서도 날짜를 계산하는 방법이 존재하기 때문에, 다른 언어들도 함께 공부해 보시는 것을 추천드립니다. 

## 관련 정보

- [C# DateTime 클래스 문서](https://docs.microsoft.com/ko-kr/dotnet/api/system.datetime?view=net-5.0)
- [C#으로 윤년을 계산하는 방법](https://www.c-sharpcorner.com/blogs/c-sharp-code-to-check-leap-year-in-net)
- [C#을 사용한 날짜 계산 예제](https://www.geeksforgeeks.org/c-sharp-date-time-examples/)