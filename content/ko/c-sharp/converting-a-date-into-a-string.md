---
title:                "날짜를 문자열로 변환하기"
html_title:           "C#: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?: 
날짜를 문자열로 변환하는 것은 프로그래머가 일일 생활에서 자주 할 수 있는 작업입니다. 왜냐하면, 우리는 날짜를 보기 쉽고 이해할 수 있도록 형식을 지정하고 싶기 때문입니다. 또한, 날짜를 다른 프로그램에 전달하거나 저장할 때도 문자열로 변환해야 합니다.

## 방법:
```C#
DateTime date = DateTime.Now;
string dateString = date.ToString("dd-MM-yyyy");
Console.WriteLine(dateString);
```
출력 결과: 06-09-2021

## 더 들어가보기:
- 날짜를 문자열로 변환하는 작업은 일반적으로 화면에 표시하기 전에 필요한 작업 중 하나입니다. 예전에는 각각의 언어에 따라 문자열 변환 방식이 다릅니다. 하지만, C#에서는 DateTime 객체의 ToString() 메소드를 통해 간단하게 형식을 지정할 수 있습니다.

- 날짜를 문자열로 변환할 때에는 다른 언어에서도 사용할 수 있는 ISO 날짜 형식 (YYYY-MM-DD)을 사용하는 것을 권장합니다. 이렇게 하면 다른 언어로 작성된 프로그램과의 호환성을 높일 수 있습니다.

- 날짜를 문자열로 변환할 때 주의할 점은 형식을 잘못 지정하면 원하는 결과가 나오지 않을 수 있다는 것입니다. 예를 들어, "mm" 대신 "MM"을 사용하면 월을 나타내는 숫자가 아니라 "분"을 나타내는 숫자가 나올 수 있습니다.

## 관련 자료:
- https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings
- https://www.c-sharpcorner.com/article/date-and-time-format-in-C-Sharp/
- https://www.c-sharpcorner.com/blogs/date-and-time-format-customization-options-in-C-Sharp