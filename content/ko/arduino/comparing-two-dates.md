---
title:                "Arduino: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜

두 날짜를 비교하는 것이 중요한 이유는 무엇일까요? 아두이노 프로그래밍에서 날짜와 시간을 다루는 것은 매우 중요합니다. 예를 들어, 센서에서 데이터를 수집할 때 언제 데이터가 수집됐는지 알아야 할 수도 있고, 특정 날짜나 시간에만 작동하도록 프로그램을 설정할 수도 있습니다. 따라서 날짜를 비교하는 방법을 알고 있으면 매우 유용합니다.

## 방법

아래에 짧은 코드 예제와 함께 "```Arduino ... ```" 코드 블록을 사용하여 날짜를 비교하는 방법을 설명하겠습니다.

```
#include <TimeLib.h> // Time 라이브러리 임포트

time_t firstDate = makeTime(0, 0, 0, 8, 1, 2021); // 첫 번째 날짜 설정, makeTime(시간, 분, 초, 일, 월, 년) 함수 사용
time_t secondDate = makeTime(0, 0, 0, 8, 5, 2021); // 두 번째 날짜 설정

if (firstDate == secondDate) { // 두 날짜가 같은지 확인
  Serial.println("두 날짜는 같습니다.");
} else if (firstDate > secondDate) { // 첫 번째 날짜가 두 번째 날짜보다 미래인지 확인
  Serial.println("첫 번째 날짜가 두 번째 날짜보다 미래입니다.");
} else { // 첫 번째 날짜가 두 번째 날짜보다 과거인지 확인
  Serial.println("첫 번째 날짜가 두 번째 날짜보다 과거입니다.");
}
```

위의 코드를 실행하면 시리얼 모니터에 결과가 나타납니다. 첫 번째 날짜가 두 번째 날짜보다 미래이기 때문에 "첫 번째 날짜가 두 번째 날짜보다 미래입니다."라는 메시지가 출력됩니다.

## 깊이 파고들기

날짜를 비교하는 데 사용되는 가장 일반적인 함수는 `makeTime()`과 `timeConversion()`입니다. `makeTime()` 함수는 날짜와 시간을 입력받아 타임스탬프를 만들어줍니다. `timeConversion()` 함수는 타임스탬프를 읽고 날짜와 시간을 출력해줍니다. 이 두 함수는 다음과 같은 형식으로 사용될 수 있습니다.

```
time_t currentTime = makeTime(0, 0, 0, 8, 1, 2021);
Serial.println(timeConversion(currentTime));
// Output: 01/08/2021 00:00:00
```

또한, 날짜를 비교할 때는 날짜를 타임스탬프로 변환해야 합니다. 이 때 `year()`과 `month()` 같은 함수를 사용하여 타임스탬프에서 원하는 날짜 정보를 추출할 수 있습니다.

## 더 보기

- [아두이노 Time 라이브러리](https://www.arduino.cc/en/Tutorial/TimeLibrary)
- [아두이노 타임스탬프 변환하기](https://programmingelectronics.com/tutorial-14-for-arduino-serial-communication-and-processing)
- [아두이노 시간 설정하기](https://learn.sparkfun.com/tutorials/time-master-tutorial/all#set-time--date-to-raspberry-pi-real-time-clock)