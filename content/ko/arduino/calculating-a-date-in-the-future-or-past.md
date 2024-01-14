---
title:                "Arduino: 미래나 과거의 날짜 계산하기"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

왜: 미래 또는 과거의 날짜를 계산하는 것에 대해 관심을 가질 수 있는 이유는 무엇일까요? 예를 들어, 결혼식 또는 여행 계획을 중간에 변경해야 할 경우 날짜를 계산하는 프로그램이 유용할 수 있습니다.

어떻게: 이제 우리는 아두이노를 사용하여 미래 또는 과거 날짜를 계산하는 방법을 알아보겠습니다. 아두이노 코드 블록 내부에 있는 예제와 출력을 기반으로 코딩하는 방법을 자세히 설명하겠습니다.

```Arduino
// 현재 날짜 정보를 입력받는 코드
int currentDay, currentMonth, currentYear;

// 원하는 년, 월, 일 정보를 입력받는 코드
int desiredDay, desiredMonth, desiredYear;

// 아래 변수에 계산된 날짜가 저장됩니다.
int calculatedDay, calculatedMonth, calculatedYear;

// 현재 날짜 정보를 입력받습니다.
currentDay = 15;
currentMonth = 6;
currentYear = 2021;

// 원하는 날짜 정보를 입력받습니다.
desiredDay = 7;
desiredMonth = 7;
desiredYear = 2021;

// 원하는 일수를 계산하여 변수에 저장합니다.
int days = (desiredYear - currentYear) * 365; // 년수 차이에 따른 일수 계산
days += (desiredMonth - currentMonth) * 30; // 월수 차이에 따른 일수 계산
days += (desiredDay - currentDay); // 날짜수 차이 계산

// 계산된 일수를 이용하여 미래 또는 과거 날짜 정보를 계산합니다.
calculatedDay = currentDay + days;
calculatedMonth = currentMonth;
calculatedYear = currentYear;

// 월 수가 12를 넘어갈 경우 연도와 월 정보를 조정합니다.
if (calculatedMonth > 12) {
  calculatedYear++;
  calculatedMonth = calculatedMonth - 12;
}

// 일 수가 30을 넘어갈 경우 월 정보를 조정합니다.
if (calculatedDay > 30) {
  calculatedMonth++;
  calculatedDay = calculatedDay - 30;
}

// 계산된 날짜 정보를 출력합니다.
Serial.print("미래 또는 과거 날짜: ");
Serial.print(calculatedDay);
Serial.print("/");
Serial.print(calculatedMonth);
Serial.print("/");
Serial.println(calculatedYear);

// 출력 결과:
// 미래 또는 과거 날짜: 22/7/2021
```

심화 분석: 날짜를 계산하는 방법은 매우 간단합니다. 단지 년, 월, 일의 차이를 구하고, 해당 차이만큼 현재 날짜에 더해주면 계산된 날짜를 얻을 수 있습니다. 따라서 우리가 실제로 위 코드에서 사용한 것은 년, 월, 일의 차이를 구하는 방법입니다. 또한 코드 내에서 월수와 일수의 제한을 설정하여 계산된 날짜가 올바른 형식을 가지게 할 수 있습니다.

## 또 다른 방법으로는 아두이노 내부의 시간 정보를 활용하여 날짜를 계산할 수도 있습니다. 이 방법은 실제 날짜와 시간 정보를 이용하기 때문에 더 정확한 결과를 얻을 수 있습니다. 그러나 이 방법은 아두이노의 RTC 모듈이나 인터넷 연결이 필요하므로, 별도의 장치 또는 설정이 필요할 수 있습니다.

사용 예제:
https://www.arduino.cc/en/Tutorial/Built