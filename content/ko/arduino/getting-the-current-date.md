---
title:                "Arduino: 현재 날짜 가져오기"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 왜

현재 날짜를 가져오는 일은 매우 유용합니다. 이를테면, 온습도 센서를 사용하는 경우, 센서 데이터를 기록할 때 현재 날짜를 함께 저장할 수 있습니다. 또는 로봇을 프로그래밍하는 경우, 오늘 날짜에 따라 다르게 동작하도록 설정할 수 있습니다.

# 어떻게

```Arduino
#include <Time.h>
#include <TimeLib.h>

void setup() {
  Serial.begin(9600);
  
  setTime(23, 59, 58, 31, 12, 2020);  // 시간을 설정합니다.

  // 현재 시간을 출력합니다.
  Serial.print("Current Time: ");
  Serial.print(hour());
  printDigits(minute());
  printDigits(second());
  Serial.print(" ");
  Serial.print(day());
  Serial.print("/");
  Serial.print(month());
  Serial.print("/");
  Serial.println(year());
}

void loop() {
  // 매 초마다 현재 시간을 업데이트합니다.
  digitalClockDisplay();
  delay(1000);
}

// 숫자가 한 자리수일 경우, 앞에 0을 추가하는 함수입니다.
void printDigits(int digits) {
  Serial.print(":");
  if(digits < 10) {
    Serial.print("0");
  }
  Serial.print(digits);
}

void digitalClockDisplay() {
  Serial.print(hour());
  printDigits(minute());
  printDigits(second());
  Serial.print(" ");
  Serial.print(day());
  Serial.print("/");
  Serial.print(month());
  Serial.print("/");
  Serial.print(year());
  Serial.println();
}

```

**출력:**

Current Time: 23:59:58 31/12/2020
0:0:0 1/1/1970
0:0:1 1/1/1970
0:0:2 1/1/1970
...
23:59:58 31/12/2020

# 깊이 파헤치기

`Time` 라이브러리를 사용하면 Arduino에서 현재 날짜와 시간을 관리할 수 있습니다. `setTime()` 함수를 사용하여 날짜와 시간을 설정할 수 있으며, `hour()`, `minute()`, `second()`, `day()`, `month()`, `year()` 함수를 사용하여 각각의 값을 가져올 수 있습니다.

하지만 주의할 점은, 시간이 Arduino가 켜질 때부터 계속 업데이트되기 때문에 전원을 끄거나 리셋할 때마다 다시 설정해주어야 합니다. 또한, 시간이 정확하지 않을 수 있으므로 외부 시간 서버와 연결하여 정확한 현재 날짜와 시간을 가져오는 것이 더 좋은 방법입니다.

# 더 알아보기

[Time 라이브러리 문서](https://github.com/PaulStoffregen/Time)

[TimeLib 라이브러리 문서](https://www.arduino.cc/en/Reference/TimeLib)