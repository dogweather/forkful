---
title:                "로깅"
date:                  2024-01-26T00:59:04.517802-07:00
model:                 gpt-4-1106-preview
simple_title:         "로깅"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/logging.md"
---

{{< edit_this_page >}}

## 무엇인가 & 왜인가?
"로깅(Logging)"은 시스템에서 일정 시간 동안 발생하는 이벤트, 거래 또는 활동에 대한 기록을 유지하는 것입니다. 프로그래머들은 로깅을 사용하여 디버깅, 시스템 건강 모니터링, 통계 수집, 심지어 사용 감사 등을 수행하며, 다양한 조건에서 코드의 동작을 이해하고 유지하는 데 없어서는 안 될 실천 방법입니다.

## 방법:
Arduino는 다른 환경처럼 내장 로깅 라이브러리를 제공하지 않지만, 최소한의 준비만으로 시리얼 콘솔에 기본 로깅을 구현할 수 있습니다. 여기 시작하는 데 도움이 되는 간단한 예제가 있습니다:

```arduino
void setup() {
  // 주어진 전송률로 시리얼 통신 시작
  Serial.begin(9600);

  // 시리얼 포트가 연결될 때까지 기다림 - 일부 보드에서만 필요
  while (!Serial) {
    ; // 시리얼 포트가 연결될 때까지 기다림. 네이티브 USB에 필요
  }

  // 설정 과정이 완료되었다는 정보 메시지 로깅
  Serial.println("Setup complete!");
}

void loop() {
  // 매 초마다 가동시간을 출력하는 간단한 로거
  static unsigned long lastLogTime = 0;
  unsigned long currentMillis = millis();

  if (currentMillis - lastLogTime >= 1000) {
    lastLogTime = currentMillis;
    Serial.print("Uptime (ms): ");
    Serial.println(currentMillis);

    // 여기에 오류 로그, 경고 또는 기타 정보를 추가할 수도 있습니다.
  }
  
  // 여기에 프로그램의 나머지 로직이 들어갑니다...
}
```

샘플 시리얼 출력:
```
Setup complete!
Uptime (ms): 1000
Uptime (ms): 2000
Uptime (ms): 3000
...
```

## 심층 탐구:
역사적으로, 마이크로컨트롤러에서의 로깅은 완전한 운영 체제에서만큼 간단하지 않았습니다. 제한된 자원은 모든 바이트가 중요했고, 개발자들은 시스템을 막히지 않게 주의해야 했습니다. 더 능력 있는 보드의 등장과 Arduino 플랫폼이 과정을 단순화함으로써 로깅이 더 접근하기 쉬워졌습니다.

위의 코드는 시리얼 인터페이스를 통한 로깅을 보여주지만, SD 카드에 기록하기, 데이터를 네트워크를 통해 원격 서버로 보내기, 작은 LCD에 출력하기 등 다른 방법도 있습니다.

로깅 시스템을 구현할 때는 로테이션, 중요도 레벨(정보, 디버그, 경고, 오류) 및 성능 영향과 같은 고려 사항이 생깁니다. Arduino에서는 로깅할 때 복잡한 데이터 구조에 대한 메모리 제약을 염두에 둘 필요가 있을 수 있습니다. 원격 로깅의 경우, 전송되는 로그의 보안도 우려사항입니다.

Arduino 세계 밖에서 널리 채택된 로깅 표준인 Syslog와 같은 더 정교한 솔루션도 있지만, 다양한 복잡성과 자원 요구 사항을 가진 유사한 기능을 제공하는 서드파티 라이브러리를 통합할 수 있습니다.

## 참조:
- [Arduino `Serial` 참조](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Arduino로 SD 카드 로깅하기](https://www.arduino.cc/en/Tutorial/LibraryExamples/Datalogger)
- [SparkFun의 데이터 로깅 실드](https://www.sparkfun.com/products/13712)
- [TinyWeb: Arduino와 원격 로깅의 실용적인 예](https://www.arduino.cc/en/Tutorial/WebClientRepeating)
