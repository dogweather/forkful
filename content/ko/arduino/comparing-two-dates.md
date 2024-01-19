---
title:                "두 날짜 비교하기"
html_title:           "C#: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

두 날짜를 비교하는 것은 특정 기간 동안의 이벤트를 추적하거나 판단하는 데 도움이 되는 프로그래밍 기법입니다. 이것은 기간이나 시간을 계산하거나 날짜가 유효한지 확인하는 데 필수적입니다.

## 방법:

아두이노(최신버전)에서 두 날짜를 비교하는 방법을 이해하기 위해 간단한 예제를 살펴봅시다.

```Arduino
#include <TimeLib.h>

time_t t1;
time_t t2;

void setup() {
  Serial.begin(9600);

  t1 = now();
  delay(10000);
  t2 = now();

  if(year(t1) == year(t2)){
     Serial.println("년 같음");
  }
  if(month(t1) == month(t2)){
     Serial.println("월 같음");
  }
  if(day(t1) == day(t2)){
     Serial.println("일 같음");
  }
}
```

위 예제에서 시간을 지연시키는 delay 함수를 사용하여 두 시간 사이의 차이를 만들었습니다. 년, 월, 일이 같은지 비교하여 출력합니다.

## 심화학습:

얼리 컴퓨터 시대에는 메모리가 제한적이어서 단순히 일자를 숫자로 표현하곤 했습니다. 하지만 이 방법은 윤년이나 다양한 월의 일수 등을 고려하지 않기 때문에 문제가 발생했습니다.

이에 대한 대안으로 타임스탬프가 도입되었습니다. 타임스탬프는 특정 시점(보통 1970년 1월 1일) 이후의 시간을 초단위로 표현하는 방식입니다.

아두이노에서는 `<TimeLib.h>` 라이브러리를 통해 날짜와 시간 관련 기능을 쉽게 사용할 수 있습니다.

## 참고자료:

코드의 예시와 주제에 대한 더 깊은 이해를 돕기 위한 링크를 제공합니다.
1. <TimeLib.h> 라이브러리 설명: (https://www.pjrc.com/teensy/td_libs_Time.html)
2. 아두이노 시간과 날짜 관련 함수: (https://www.arduino.cc/reference/en/libraries/time/)
3. 유닉스 타임스탬프에 대한 설명: (https://en.wikipedia.org/wiki/Unix_time)