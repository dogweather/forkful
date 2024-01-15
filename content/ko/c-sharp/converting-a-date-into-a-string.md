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

## 왜
날짜를 문자열로 변환하는 작업을 하는 이유는 시간과 날짜를 포함한 데이터를 시스템에서 표현하고 조작하기 쉽기 때문입니다.

## 어떻게
변환을 위해 .NET Framework에서 제공하는 DateTime의 ToString 메서드를 사용합니다. 이 메서드는 날짜 형식을 정의하는 형식 문자열을 매개변수로 받아 해당하는 형식으로 날짜를 문자열로 변환합니다.
```C#
DateTime date = new DateTime(2021, 10, 31);
Console.WriteLine(date.ToString("MM/dd/yyyy"));
// 결과: 10/31/2021
```
지원되는 형식 문자열에는 "MM", "dd", "yyyy"와 같은 날짜 포맷팅 문자열뿐만 아니라 "dddd"와 같은 요일 포맷팅 문자열도 있습니다. 더 많은 기능과 예제 코드는 아래의 링크를 참고하세요.

## 깊게 파헤치기
DateTime의 ToString 메서드는 형식 문자열 외에도 IFormatProvider 인터페이스를 구현한 클래스의 인스턴스를 매개변수로 받을 수 있습니다. 이 인스턴스는 날짜 형식을 지정할 뿐만 아니라 다른 언어나 지역에 맞게 날짜를 변환할 때도 사용됩니다. 또한 .NET 5 이상에서는 "Standard Date and Time Format Strings"를 통해 일반적으로 사용되는 날짜 형식 문자열을 제공합니다.

## 관련 정보
[Microsoft 공식 문서 - DateTime.ToString 메서드](https://docs.microsoft.com/ko-kr/dotnet/api/system.datetime.tostring)