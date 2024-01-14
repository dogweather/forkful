---
title:    "Arduino: 미래나 과거 날짜 계산하기"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## 왜
날짜를 미래나 과거로 계산할 필요성
날짜를 미래나 과거로 계산하는 것은 다양한 이유가 있을 수 있습니다. 예를 들어 아두이노를 사용하여 센서 데이터를 수집하고 특정 날짜와 시간에 이를 활용할 수 있습니다. 또한 특정 이벤트나 행사를 계획하는 데에도 필요한 기능입니다.

## 사용 방법
아두이노에서 날짜를 미래나 과거로 계산하기 위해서는 ```day(), month(), year()``` 함수와 연산자를 사용해야 합니다. 예를 들어, 현재 날짜와 10일 후의 날짜를 계산하는 코드는 다음과 같습니다.

```Arduino
int currentDay = day(); // 현재 날짜 저장
int futureDay = currentDay + 10; // 10일 후의 날짜 계산
int currentMonth = month(); // 현재 월 저장
int currentYear = year(); // 현재 연도 저장
```

실행 결과는 다음과 같이 나타납니다.

```Arduino
Current date: 7/11/2021
Future date: 7/21/2021
```

## 깊이 알아보기
날짜를 계산하는 방법에는 여러 가지가 있지만, 가장 간단한 방법은 현재 날짜를 변수에 저장하고 이 변수를 수정해가면서 미래나 과거로 계산하는 것입니다. 또한 시간까지 고려해야 할 경우에는 ```hour(), minute(), second()``` 함수를 활용하여 이를 함께 계산할 수 있습니다.

또 다른 방법으로는 시간과 날짜를 저장하기 위해 자료형을 사용하는 방법이 있습니다. 예를 들어, ```struct```를 사용하여 연도, 월, 일, 시간, 분, 초를 저장할 수 있습니다. 이를 활용하면 보다 정확하고 다양한 날짜 계산을 할 수 있습니다.

## 연관 자료
Github에서 제공하는 라이브러리를 활용하면 더 많은 기능을 포함한 날짜 계산이 가능합니다. 아래의 링크를 참고해보세요.

[Arduino 날짜 시간 라이브러리](https://github.com/PaulStoffregen/Time)

## 참고 자료
- [Arduino Reference - day()](https://www.arduino.cc/reference/en/language/functions/time/day/)
- [Arduino Reference - month()](https://www.arduino.cc/reference/en/language/functions/time/month/)
- [Arduino Reference - year()](https://www.arduino.cc/reference/en/language/functions/time/year/)
- [C++ struct 사용 방법](https://www.learncpp.com/cpp-tutorial/47-structs/)