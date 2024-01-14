---
title:    "Arduino: 현재 날짜 가져오기"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# 왜: 현재 날짜를 얻는 것에 참여해야 하는 이유
현재 날짜를 올바르게 추적하는 것은 매우 중요합니다. 소셜 미디어에 게시물을 할 수 있을 뿐만 아니라 일일 업무 계획을 작성하거나 프로젝트 일정을 관리하기 위해서도 현재 날짜를 알고 있어야 합니다.

# 어떻게: 날짜 정보를 얻는 방법
아두이노 보드를 사용하면 현재 날짜 정보를 얻는 것이 매우 간단합니다. 아래의 코드 블록을 참조하여 현재 날짜를 출력해 보세요.

```Arduino
#include <RTClib.h> // RTC 라이브러리 불러오기
RTC_DS1307 rtc; // RTC 객체 생성

void setup()
{
  Serial.begin(9600); // 시리얼 통신 시작
  rtc.begin(); // RTC 시계 시작
}

void loop()
{
  DateTime now = rtc.now(); // 현재 날짜 정보를 now 변수에 저장
  Serial.print(now.day(), DEC); // 현재 날짜 정보 중 일 출력
  Serial.print('/'); // 슬래시로 구분
  Serial.print(now.month(), DEC); // 현재 날짜 정보 중 월 출력
  Serial.print('/'); // 슬래시로 구분
  Serial.print(now.year(), DEC); // 현재 날짜 정보 중 년도 출력
  Serial.println(); // 개행
  delay(1000); // 1초 대기
}
```

만약 시리얼 모니터 상에서 월과 년도의 출력이 10 미만의 수로 표시되는 경우, 필요에 따라 `Serial.print()` 함수를 사용하여 앞에 0을 추가할 수 있습니다.

# 깊이 들어가기: 현재 날짜 정보에 대한 더 깊은 이해
아두이노에서 현재 날짜 정보를 얻는 것은 RTC (Real Time Clock) 모듈을 사용함으로써 가능합니다. RTC 모듈에는 시계 동작에 필요한 일반적인 하드웨어 구성 요소가 포함되어 있으며, 아두이노와 연결되어 사용됩니다.

RTC 모듈에는 시계에 필요한 일반적인 하드웨어 요소 외에도 NVRAM (Non-Volatile Random Access Memory), 배터리 등이 포함되어 있습니다. NVRAM은 정보를 저장하고, 배터리는 정전이나 전원 공급 문제가 발생했을 때 정보가 지워지지 않도록 합니다. 따라서 RTC 모듈을 사용하면 전원이 꺼져도 시계가 멈추지 않고 계속 작동합니다.

# 관련 항목
- RTC 모듈을 사용한 실시간 시계 작성하기: https://www.arduino.cc/en/Tutorial/BuiltInExamples/Melody
- RTC 사용 예제: https://learn.adafruit.com/ds1307-real-time-clock-breakout-board-kit/using-the-rtc
- RTC 모듈 구매링크: https://www.aliexpress.com/wholesale?SearchText=RTC+module