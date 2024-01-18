---
title:                "문자열에서 날짜 파싱하기"
html_title:           "Arduino: 문자열에서 날짜 파싱하기"
simple_title:         "문자열에서 날짜 파싱하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 무엇인가요? 
날짜를 문자열에서 추출하는 것이 무엇인지 그리고 프로그래머들이 왜 이런 작업을 하는지 두 개 sentences로 설명합니다. 

## 하는 법: 
```Arduino 
// 이 예제는 문자열에서 날짜를 추출하는 방법을 보여줍니다.
String date = "2021/05/25"; // 추출하고자 하는 문자열 
String year = date.substring(0,4); // 연도를 추출합니다. 
String month = date.substring(5,7); // 월을 추출합니다.
String day = date.substring(8); // 일을 추출합니다.

Serial.print("Date: "); 
Serial.print(year); // 연도를 출력합니다. 
Serial.print("/"); 
Serial.print(month); // 월을 출력합니다. 
Serial.print("/"); 
Serial.println(day); // 일을 출력합니다. 

// 출력 결과:
// Date: 2021/05/25
``` 

## 자세히 알아보기: 
(1) parsing이라는 작업은 비록 초보 프로그래머에게는 생소할 수 있지만, 날짜를 추출하는 것은 우리의 일상생활에서 충분히 익숙한 작업입니다. (2) 이 작업을 하기 위해 다른 언어나 라이브러리를 사용할 수도 있지만, Arduino에서 제공하는 string 클래스를 사용하면 간단하게 날짜를 추출할 수 있습니다. (3) 문자열에서 날짜를 추출하기 위해서는 문자열의 포맷과 추출하려는 날짜의 위치를 고려해야 합니다. 위의 예제에서는 '/'를 구분자로 사용하여 연도, 월, 일을 추출하였습니다.

## 더 알아보기: 
- <https://www.arduino.cc/en/Reference/StringSubstring> 
- <https://en.wikipedia.org/wiki/Date_format_by_country> 

** Note: This is a rough translation and may need to be proofread by a native speaker for accuracy **