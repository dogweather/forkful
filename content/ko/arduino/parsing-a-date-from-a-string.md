---
title:                "문자열에서 날짜 파싱하기"
date:                  2024-01-20T15:34:34.431721-07:00
simple_title:         "문자열에서 날짜 파싱하기"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열에서 날짜를 파싱한다는 것은, 날짜와 시간 정보를 담은 텍스트를 구조화된 데이터로 변환하는 과정입니다. 프로그래머들은 데이터 처리와 시간 관리를 위해 이 작업을 수행합니다.

## How to: (어떻게 하나요?)
Arduino에서는 표준 라이브러리로 제공되는 `String` 클래스와 C++의 함수들을 이용해 문자열에서 날짜를 파싱할 수 있습니다. 예를 들어, "2023-04-05"와 같은 날짜 문자열을 연(year), 월(month), 일(day)로 분리할 수 있습니다.

```c++
String dateStr = "2023-04-05"; // 날짜 문자열
int year, month, day;

void setup() {
  Serial.begin(9600);
  parseDate(dateStr, year, month, day);
  Serial.print("Year: ");
  Serial.println(year);   // Year: 2023
  Serial.print("Month: ");
  Serial.println(month);  // Month: 4
  Serial.print("Day: ");
  Serial.println(day);    // Day: 5
}

void loop() {
  // 여기서는 아무 작업도 수행하지 않습니다.
}

void parseDate(String date, int &y, int &m, int &d) {
  y = date.substring(0, 4).toInt();
  m = date.substring(5, 7).toInt();
  d = date.substring(8, 10).toInt();
}
```

## Deep Dive (심층 분석)
문자열에서 날짜 파싱은 일반적으로 C++의 표준 라이브러리 또는 Arduino용으로 개발된 타사 라이브러리를 사용합니다. 이 예에서는 `String` 클래스의 `substring()`과 `toInt()` 메서드를 통해 구현합니다. `substring()`은 문자열의 특정 부분을 추출하고, `toInt()`는 해당 문자열을 정수로 변환합니다.

과거엔 C언어의 `sscanf()`나 `strtok()`과 같은 함수를 사용한 파싱 방법이 많았으나, Arduino에서는 `String` 클래스의 메서드들이 그 역할을 대신하고 있습니다. 다만, `String` 객체 사용 시 메모리를 많이 요구하고, 메모리 파편화를 일으킬 수 있어 큰 프로젝트에서는 더 효율적인 자료 구조를 고려하는 것이 좋습니다.

## See Also (더보기)
- Arduino `String` 클래스: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- C++ `std::stoi` 컨버팅 함수: https://en.cppreference.com/w/cpp/string/basic_string/stol
- 시간 관련 프로젝트에 사용되는 타사 라이브러리인 `TimeLib.h`: https://github.com/PaulStoffregen/Time
