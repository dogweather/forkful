---
title:    "C#: 날짜를 문자열로 변환하기"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜

날짜를 문자열로 전환하는 것에 대해 생각한 적이 있습니까? 프로그래밍에서 날짜 형식을 사용하는 것은 꽤 흔하기 때문에, 많은 사람들은 날짜를 문자열로 변환하는 기술을 사용하게 됩니다. 이로 인해 코드를 읽는 데 도움이 될 수 있고, 데이터베이스나 파일과 같은 외부 소스와의 교류도 가능합니다.

## 방법

만약 여러분이 날짜를 문자열로 변환하는 법을 찾고 있다면, 당신은 운이 좋습니다. 우리는 여러분에게 C#에서 날짜를 문자열로 변환하는 쉬운 방법을 소개하겠습니다.

```
C# 
DateTime date = new DateTime(2020, 6, 10);

// 가장 일반적인 날짜 형식으로 변환
string dateString = date.ToString("d");

// "6/10/2020" 출력
Console.WriteLine(dateString);

// 사용자 정의 형식으로 변환
string customDateString = date.ToString("yyyy-MM-dd");

// "2020-06-10" 출력
Console.WriteLine(customDateString);
```

위의 예제 코드를 보면, 우리는 굳이 날짜를 문자열로 변환하려면 `ToString()` 메서드를 사용하면 된다는 것을 알 수 있습니다. 이 메서드는 매개변수로 날짜를 문자열로 변환할 형식을 받게 되며, 날짜를 원하는 식으로 나타낼 수 있습니다.

## 깊이 파고들기

`ToString()` 메서드는 날짜를 문자열로 변환하는 가장 일반적이면서도 간단한 방법입니다. 그러나, 여러분이 더 복잡한 형식의 날짜를 원한다면 어떨까요? 이를 위해서는 날짜 포매터를 생성해야 합니다. 날짜 포매터는 날짜를 문자열로 변환하기 위해 사용되는 클래스입니다. 아래는 `DateTime` 변수를 사용하여 날짜를 문자열로 변환하는 예제 코드입니다.

```
C#
DateTime date = new DateTime(2020, 6, 10);

// 날짜 포매터 생성
DateTimeFormatter formatter = new DateTimeFormatter("yyyyMMdd");

// 날짜를 문자열로 변환
string dateString = formatter.Format(date);

// "20200610" 출력
Console.WriteLine(dateString);
```

위의 예제 코드에서 우리는 먼저 `DateTimeFormatter` 클래스를 사용하여 `yyyyMMdd` 형식의 날짜 포매터를 생성했습니다. 그리고 `Format()` 메서드를 사용하여 `DateTime` 변수를 문자열로 변환했습니다. 이렇게 하면 우리는 더 복잡한 날짜 형식을 원하는대로 나타낼 수 있습니다.

## 관련 정보

- [C# DateTime.ToString() 메서드 문서](https://docs.microsoft.com/ko-kr/dotnet/api/system.datetime.tostring?view=netcore-3.1)
- [C# DateTime 포맷 문자열 문서](https://docs.microsoft.com/ko-kr/dotnet/standard/base-types/custom-date-and-time-format-strings)
- [C# DateTime 클래스 문서](https://docs.microsoft.com/ko-kr/dotnet/api/system.datetime?view=netcore-3.1)

## 참고

날짜를 문자열로 변환하는 것은 매우 유용한 기술입니다. 여러분이 날짜 형식을 사용하는 프로그래밍 작업을 할 때, 이 기술을 활용해보세요. 그리고 우리의 이 블로그 포스트를 참고해 주셔서 감사합니다!