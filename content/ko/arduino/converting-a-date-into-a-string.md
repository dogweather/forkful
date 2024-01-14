---
title:                "Arduino: 날짜를 문자열로 변환하기"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# 왜: 날짜를 문자열로 변환하는 것에 참여하는 이유는?

날짜를 문자열로 변환하는 것은 Arduino 프로그래밍에서 매우 유용합니다. 예를 들어, 센서를 이용해 얻은 날짜 정보를 표시하고 싶을 때, 날짜를 문자열로 변환할 수 있다면 쉽게 표시할 수 있습니다. 또는 시간과 관련된 다른 작업을 할 때도 도움이 될 수 있습니다. 여러분이 무엇을 하고 싶든, 날짜를 문자열로 변환하는 것은 더 쉬운 일이 될 것입니다.

# 사용 방법:

1. **날짜 정보 설정하기**: 날짜와 시간을 표현하는 변수를 설정해야 합니다. 예를 들어, `int day = 24`, `int month = 12`, `int year = 2020`, `int hour = 18`, `int minute = 30`, `int second = 0` 과 같이 변수를 설정할 수 있습니다.

```
Arduino
int day = 24;
int month = 12;
int year = 2020;
int hour = 18;
int minute = 30;
int second = 0;
```

2. **문자열 변수 설정하기**: `String` 함수를 사용해 문자열로 변환한 날짜 정보를 저장할 변수를 설정합니다. 예를 들어, `String date = "";`를 사용할 수 있습니다.

```
Arduino
String date = "";
```

3. **날짜를 문자열로 변환하기**: `String` 변수에 `Int` 변수를 연결하는 방법을 사용해 날짜를 문자열로 변환합니다. 예를 들어, `date = String(month) + "/" + String(day) + "/" + String(year) + " " + String(hour) + ":" + String(minute) + ":" + String(second);` 와 같은 방법으로 날짜를 문자열로 변환할 수 있습니다.

```
Arduino
date = String(month) + "/" + String(day) + "/" + String(year) + " " + String(hour) + ":" + String(minute) + ":" + String(second);
```

4. **문자열 출력하기**: `Serial.println()` 함수를 사용해 문자열을 시리얼 모니터에 표시할 수 있습니다. 예를 들어, `Serial.println(date);`를 사용하면 `12/24/2020 18:30:00`과 같은 형식으로 시리얼 모니터에 날짜가 표시됩니다.

```
Arduino
Serial.println(date);
```

# 딥 다이브:

*날짜를 문자열로 변환하는 더 많은 방법이 있는지 궁금하십니까?*

날짜를 문자열로 변환하는 다른 방법 중에는 `sprintf()` 함수를 사용하는 방법이 있습니다. 이 함수는 C 언어에서 사용되는 가변 인수 함수로서, 다양한 형식의 값을 하나의 문자열로 합쳐주는 역할을 합니다. 다음은 `sprintf()` 함수를 사용해 날짜를 문자열로 변환하는 예제 코드입니다.

```
Arduino
#include <stdio.h>

int day = 24;
int month = 12;
int year = 2020;
int hour = 18;
int minute = 30;
int second = 0;

char date[20];
sprintf(date, "%02d/%02d/%04d %02d:%02d:%02d", month, day, year, hour, minute, second);
```

위의 코드는 이전 예제와 동일한 결과를 출력하지만, 더 많은 형식을 제공할 수 있기 때문에 더 유용합니다.

# 비슷한 작업 참고:

- [Arduino String 함수](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/)
- [sprintf() 함수 정보](https://www.tutorialspoint.com/c_standard_library/c_function_sprintf.htm)
- [C