---
title:    "Arduino: 두 날짜 비교하기"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# 왜 비교를 해야 할까?

두 날짜를 비교하는 것이 왜 중요한지 궁금하신가요? 두 날짜를 비교함으로써 우리는 시간과 날짜를 더 쉽게 다룰 수 있게 됩니다. 예를 들어, 우리가 특정 기간 동안 센서에서 수집한 데이터를 분석하고자 한다면 날짜 비교는 매우 유용합니다. 따라서 두 날짜를 비교하는 방법에 대해 알아보도록 하겠습니다.

## 어떻게 하는가?

아두이노에서는 ```if```문을 사용해서 두 날짜를 비교할 수 있습니다. 먼저 날짜와 시간을 저장하기 위해 다음과 같이 변수를 선언해줍니다.

```
ArduinoDateTime date1;
ArduinoDateTime date2;
```

이제 두 날짜를 비교해보겠습니다. 만약 첫 번째 날짜가 두 번째 날짜보다 이전이라면 LED를 켜도록 간단한 예제 코드를 작성해보겠습니다.

```
if (date1 < date2) { // 날짜 비교
  digitalWrite(LED_PIN, HIGH); // LED 켜기
}
```

그리고 두 날짜가 같은지를 확인하고 싶으면 아래와 같이 작성하면 됩니다.

```
if (date1 == date2) { // 날짜 비교
  Serial.println("두 날짜는 같습니다."); // 시리얼 모니터에 출력
}
```

위의 예제 코드에서는 ```<```와 ```==``` 연산자를 사용해서 날짜들을 비교했습니다. 비교 연산자를 사용할 때는 ```if```문을 사용하거나, ```while``` 루프 내에서 사용할 수 있습니다. 하지만 날짜를 일련의 숫자로 저장하고 싶다면, 날짜를 ```int```로 변환한 다음에 비교해야 합니다.

이제 위의 예제 코드를 바탕으로 샘플 출력 코드를 작성해 보겠습니다.

```
#include <RTClib.h>

RTC_DS1307 rtc;

void setup () {
  Serial.begin(9600);
  if (! rtc.begin()) {
    Serial.println("RTC 초기화 실패!");
  }
    Serial.println("현재 날짜와 시간을 출력합니다.");
    if (rtc.lostPower()) {
      Serial.println("날짜가 잃어버렸습니다.");
      rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
    }
  rtc.adjust(DateTime(2020, 11, 1, 12, 0, 0));
}

DateTime date1(2020, 11, 1, 12, 0, 0); // 첫 번째 날짜
DateTime date2(2020, 11, 30, 12, 0, 0); // 두 번째 날짜

void loop () {
  if (date1 < date2) { // 날짜 비교
    digitalWrite(LED_PIN, HIGH); // LED 켜기
    Serial.println("첫 번째 날짜는 두 번째 날짜보다 이전입니다.");
  }
  if (date1 == date2) { // 날짜 비교
    Serial.println("두 날짜는 같습니다.");
  }
  delay(1000);
}
```

위의 코드를 실행하면 시리얼 모니터에 날짜 비교 결과가 출력됩니다.

```
첫 번째 날짜는 두 번째 날짜보다 이전입니다.
```

## 깊이 파고들기

날짜 비교를 할 때, 두 날짜의 정확한 형식을 지켜주어야 합니다. 아두이노