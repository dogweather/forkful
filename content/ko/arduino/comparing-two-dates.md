---
title:                "Arduino: 두 날짜 비교하기"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

# 왜

두 날짜를 비교하는 것이 왜 중요한지 궁금하신가요? 다양한 프로젝트에서 날짜 비교는 매우 유용합니다. 예를 들어 센서 데이터를 수집하는 기기를 만들 경우, 데이터 수집 시간을 기준으로 이전 데이터와 비교하여 웨어러블 장비의 건강 상태를 판단할 수 있습니다. 또한 이전 날짜와 현재 날짜를 비교하여 특정 장치가 작동한 시간을 계산하는 등의 용도로도 사용할 수 있으며, 다양한 아두이노 프로젝트에서 필요한 날짜 비교 기능을 구현할 수 있습니다.

# 어떻게

날짜를 비교하기 위해서는 시간을 나타내는 변수를 최소한 두 개 필요합니다. 예를 들어, 현재 시간을 나타내는 변수를 "now"라고 하고, 비교할 이전 시간을 나타내는 변수를 "prev"라고 한다면, 다음과 같이 코드를 작성할 수 있습니다.

```Arduino
unsigned long prev = millis(); // 이전 시간 설정
unsigned long now = millis(); // 현재 시간 설정
```

각 변수는 unsigned long 형으로 선언되어야 하며, millis() 함수는 아두이노 보드가 실행된 시간을 밀리초 단위로 반환하는 함수입니다. 이제 비교를 위해 if문을 사용하여 prev와 now의 값을 비교할 수 있습니다.

```Arduino
if(now - prev > 1000) { // 1000밀리초(1초)가 지났을 때
  // 이전 시간과 현재 시간을 비교하는 코드를 작성합니다.
  // 예를 들어, Serial Monitor를 사용하여 이전 시간과 현재 시간을 출력할 수 있습니다.
  Serial.print("이전 시간: ");
  Serial.println(prev);
  Serial.print("현재 시간: ");
  Serial.println(now);
  
  prev = now; // 이전 시간을 현재 시간으로 재설정
}
```

위 코드에서는 prev와 now의 값이 1초 이상 차이가 나면 이전 시간과 현재 시간을 Serial Monitor를 통해 출력하고, 이전 시간을 현재 시간으로 갱신합니다. 이를 통해 매번 알아보기 힘든 시간과 날짜를 쉽게 비교할 수 있습니다.

# 깊이 파고들기

위에서는 millis() 함수를 사용하여 아두이노 보드가 실행된 시간을 밀리초 단위로 구하였지만, 실제로는 운영 체제에서 시간을 관리하고 있기 때문에 정확한 시간 비교를 위해서는 time 라이브러리를 사용해야 합니다. 이 라이브러리를 사용하면 현재 시간을 변수로 저장할 수 있으며, 특정 시간 형식으로 변환하여 비교할 수도 있습니다.

또 다른 방법으로는 RTC(Real-time clock)를 사용하는 것입니다. RTC는 시간과 날짜를 정확하게 유지하면서 아두이노 보드가 꺼져 있어도 시간을 계속 유지할 수 있도록 동작하는 장치입니다. 이를 사용하면 보다 정확한 날짜 비교를 할 수 있습니다.

# 이 외에도 참고할 만한 자료

- [Time 라이브러리 공식 문서](https://www.arduino.cc/en/Reference/Time)
- [Arduino Time Library 사용법 예제](https://www.arduino.cc/en/Tutorial/Time)
- [RTC 모듈을 사용한 아