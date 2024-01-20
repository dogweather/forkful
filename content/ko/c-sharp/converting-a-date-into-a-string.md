---
title:                "날짜를 문자열로 변환하기"
html_title:           "Arduino: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 무엇이며 왜합니까?

날짜를 문자열로 변환하는 것은 날짜 데이터가 포함된 변수를 문자열 형식으로 바꾸는 것을 말합니다. 이는 프로그래머가 날짜 데이터를 좀 더 보기 쉽도록 표현하거나 다른 시스템과 통신하기 위해 필요합니다.

## 어떻게?

C#을 사용하여 날짜를 문자열로 변환하는 것은 매우 간단합니다. 첫 번째 예제를 보겠습니다.
```C#
DateTime myDate = DateTime.Now;
string dateInString = myDate.ToString("MM/dd/yyyy");
Console.WriteLine(dateInString); 
```
이 코드를 실행하면 오늘 날짜를 `MM/dd/yyyy` 형식의 문자열로 출력합니다.

## 딥 다이브

1. 역사적 맥락: C#에는 날짜를 문자열로 표현하는 데 사용할 수 있는 다양한 메소드와 포맷이 있습니다.
2. 대안: `DateTime.ToString()` 외에도 `String.Format()`이나 문자열 보간을 사용할 수 있습니다.
```C#
DateTime myDate = DateTime.Now;
string dateInString = string.Format("{0:MM/dd/yyyy}", myDate);
Console.WriteLine(dateInString);
```
3. 구현 세부 사항 : `ToString()`은 현재 시스템의 문화권에 따라 출력을 제공합니다. 직접 특정 형식을 지정하려면 `ToString("format")`을 사용합니다.

## 참고문헌

- [DateTime.ToString() 메서드 (Microsoft 공식 문서)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tostring?view=net-5.0)
- [String.Format 메서드 (Microsoft 공식 문서)](https://docs.microsoft.com/en-us/dotnet/api/system.string.format?view=net-5.0)
- [문자열 보간 (Microsoft 공식 문서)](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/interpolated)