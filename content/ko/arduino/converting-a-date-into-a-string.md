---
title:    "Arduino: 날짜를 문자열로 변환하기"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

### 왜 이것을 진행하나요?

Arduino 프로그래밍을 하는 동안 날짜를 문자열로 변환해야 할 때가 있습니다. 예를 들어, 센서 데이터의 타임스탬프를 저장하거나 사용자에게 날짜 정보를 보여줄 때 등등 다양한 경우가 있을 수 있습니다.

### 어떻게 이루어지나요?

아두이노에서 날짜를 문자열로 변환하는 방법은 아주 간단합니다. 우선, `Day`, `Month`, `Year` 변수를 각각 해당 날짜의 일, 월, 년으로 설정합니다. 그리고 `Serial.print()` 함수를 사용하여 원하는 형식의 문자열로 날짜를 출력하면 됩니다. 아래 예시 코드를 참고해주세요.

```Arduino
int day = 10; // 날짜 설정 
int month = 5; // 월 설정
int year = 2021; // 년 설정

// 날짜를 원하는 형식으로 출력하기 
Serial.print(day); // "10" 출력 
Serial.print("/"); // "/" 출력 
Serial.print(month); // "5" 출력 
Serial.print("/"); // "/" 출력 
Serial.print(year); // "2021" 출력
```

위 예제 코드를 실행하면, 시리얼 모니터에 "10/5/2021"이라는 문자열이 출력될 것입니다.

### 깊이 파고들어보기

날짜를 문자열로 변환하는 방법은 매우 간단하지만, 조금 더 알아볼 만한 내용이 있습니다. 첫 번째로, `Serial.print()` 함수는 여러 가지 변수를 함께 출력할 수 있습니다. 예를 들어, 아래 예제 코드를 실행하면 날짜, 시간, 온도 등 다양한 정보를 한 줄에 출력할 수 있습니다.

```Arduino
int day = 10; // 날짜 설정 
int month = 5; // 월 설정
int year = 2021; // 년 설정
int hour = 12; // 시간 설정
int minute = 30; // 분 설정
int second = 45; // 초 설정
int temperature = 25; // 온도 설정 

// 날짜, 시간, 온도 한 줄에 출력하기 
Serial.print(day); // "10" 출력 
Serial.print("/"); // "/" 출력 
Serial.print(month); // "5" 출력 
Serial.print("/"); // "/" 출력 
Serial.print(year); // "2021" 출력
Serial.print(" - "); // " - " 출력 
Serial.print(hour); // "12" 출력 
Serial.print(":"); // ":" 출력 
Serial.print(minute); // "30" 출력 
Serial.print(":"); // ":" 출력 
Serial.print(second); // "45" 출력
Serial.print(" - Temperature: "); // " - Temperature: " 출력 
Serial.print(temperature); // "25" 출력 
Serial.print("°C"); // "°C" 출력
```

출력 결과는 "10/5/2021 - 12:30:45 - Temperature: 25°C"가 될 것입니다. 이처럼 여러 가지 변수를 출력할 때 `Serial.print()` 함수와 문자열을 결합하여 사용할 수 있습니다.

또 다른 팁은 `Serial.println()` 함수를 사용하는 것입니다. `println()` 함수는 `print()` 함수와 같지만, 출력 내용의 끝에 자동으로 개행 문자(\n)를 붙여 줍니다. 위 예제 코드를 `println()` 함수로 바꿔서 실행하면 시리얼 모니터에 한 줄에 출력되던 내용이 각각의 줄로 나누어져 출력됩니다.

이처럼 아두이노에서 날짜를 문자열로 변환하는 방법은 매우 쉽고 다양하게 응용할 수 있습니다. 참고로 아두이노에는 시간과 날짜를 다루는 라이브