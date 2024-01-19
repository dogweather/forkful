---
title:                "디버그 출력을 인쇄하기"
html_title:           "Clojure: 디버그 출력을 인쇄하기"
simple_title:         "디버그 출력을 인쇄하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## 무엇 그리고 왜?

디버그 출력은 실시간으로 코드의 동작을 통제하고 이해하는 방법입니다. 프로그래머들은 오류를 찾고 해결하며, 기능을 확인하기 위해 이를 사용합니다.

## 어떻게 할까요:

디버그 출력은 쉽게 가능합니다. 기본 Serial.println() 함수를 사용하여 세부 정보를 콘솔에 출력하면 됩니다.

```Arduino
void setup() {
    Serial.begin(9600); // 시작 시 시리얼 통신을 설정, 9600은 통신 속도
}

void loop() {
    int sensorValue = analogRead(A0); // 센서에서 값을 읽음
    Serial.println(sensorValue); // 시리얼 모니터에 값 출력
    delay(200); // 200ms를 대기
}
```

이 코드를 실행하면, 시리얼 모니터에 센서값을 출력하게 될 것입니다.


## 깊이 들어가기:

디버그 출력은 프로그래밍의 오랜 역사를 가지고 있으며, 처음에는 프로그램의 오류를 식별하고 수정하는 것이 주로 사용되었습니다. 

검출도구나 시각적 디버거 같은 대안들이 있지만, 디버그 출력은 여전히 강력한 도구로 남아있습니다.

동작은 단순합니다. Serial.begin()은 시리얼 포트를 연 후 시리얼 통신을 시작합니다. 그 다음 Serial.println() 함수를 이용하여 메시지를 출력합니다.


## 참고자료:

Arduino의 디버깅에 대한 추가 정보는 아래 링크를 참조하세요. https://www.arduino.cc/en/Serial

디버그 전략에 대한 더욱 자세한 내용은 다음을 참조하세요. https://learn.adafruit.com/adafruit-arduino-lesson-6-digital-inputs/further-reading