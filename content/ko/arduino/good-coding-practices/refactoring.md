---
date: 2024-01-26 01:17:03.903519-07:00
description: "\uB9AC\uD329\uD1A0\uB9C1\uC740 \uCF54\uB4DC\uC758 \uC678\uBD80 \uB3D9\
  \uC791\uC774\uB098 \uAE30\uB2A5\uC744 \uBCC0\uACBD\uD558\uC9C0 \uC54A\uC73C\uBA74\
  \uC11C \uCF54\uB4DC\uB97C \uC7AC\uC791\uC5C5\uD558\uC5EC \uAD6C\uC870\uC640 \uAC00\
  \uB3C5\uC131\uC744 \uAC1C\uC120\uD558\uB294 \uACFC\uC815\uC785\uB2C8\uB2E4. \uD504\
  \uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uCF54\uB4DC\uB97C \uB354\uC6B1 \uAE68\uB057\
  \uD558\uACE0 \uC774\uD574\uD558\uAE30 \uC27D\uACE0 \uC720\uC9C0\uBCF4\uC218\uD558\
  \uAE30 \uC27D\uAC8C \uB9CC\uB4E4\uAE30 \uC704\uD574 \uB9AC\uD329\uD1A0\uB9C1\uC744\
  \ \uD569\uB2C8\uB2E4. \uC7A5\uAE30\uC801\uC73C\uB85C \uBCF4\uBA74 \uB514\uBC84\uAE45\
  \uACFC \uC0C8\uB85C\uC6B4 \uAE30\uB2A5 \uCD94\uAC00\uB97C \uD6E8\uC52C \uB35C \uACE8\
  \uCE58\u2026"
lastmod: '2024-03-13T22:44:55.619845-06:00'
model: gpt-4-0125-preview
summary: "\uB9AC\uD329\uD1A0\uB9C1\uC740 \uCF54\uB4DC\uC758 \uC678\uBD80 \uB3D9\uC791\
  \uC774\uB098 \uAE30\uB2A5\uC744 \uBCC0\uACBD\uD558\uC9C0 \uC54A\uC73C\uBA74\uC11C\
  \ \uCF54\uB4DC\uB97C \uC7AC\uC791\uC5C5\uD558\uC5EC \uAD6C\uC870\uC640 \uAC00\uB3C5\
  \uC131\uC744 \uAC1C\uC120\uD558\uB294 \uACFC\uC815\uC785\uB2C8\uB2E4."
title: "\uB9AC\uD329\uD1A0\uB9C1"
weight: 19
---

## 어떻게 할까?
아두이노에서 너무 많은 일을 하는 함수가 있다고 가정해 보겠습니다. 예를 들어 이런 상황입니다:

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  // 너무 많은 일을 하는 함수
  handleEverything();
}

void handleEverything() {
  // 센서 데이터 읽기
  int sensorValue = analogRead(A0);
  // 센서 데이터 처리하기
  sensorValue = map(sensorValue, 0, 1023, 0, 255);
  // 센서 데이터 출력하기
  Serial.println(sensorValue);
  delay(500);
}
```

이를 리팩토링하면 `handleEverything()`을 더 작고, 더 집중된 함수로 분할하는 것처럼 보일 수 있습니다:

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  int sensorValue = readSensorData();
  int processedValue = processSensorData(sensorValue);
  printData(processedValue);
  delay(500);
}

int readSensorData() {
  return analogRead(A0);
}

int processSensorData(int sensorValue) {
  return map(sensorValue, 0, 1023, 0, 255);
}

void printData(int data) {
  Serial.println(data);
}
```

리팩토링 후에는 `loop()` 함수가 더 읽기 쉬워지고 각 작업이 전용 함수에 의해 처리되어 코드를 더 쉽게 관리할 수 있게 됩니다.

## 심층 탐구
역사적으로, 리팩토링은 지속적인 코드 개선을 통해 변경 요구사항에 적응하는 애자일 및 테스트 주도 개발(TDD) 방법론의 부상과 함께 인기를 얻었습니다. 아두이노 예제에서 사용된 "메서드 추출" 기법과 같은 다양한 도구와 전략을 통해 리팩토링을 할 수 있습니다. 이는 빠른 프로토타입에서 안정된 프로젝트로 넘어갈 때, 코드의 가독성과 유지보수가 중요해질 때 필수적입니다.

리팩토링을 할 때는 변경이 버그를 도입하지 않았는지 확인하기 위해 좋은 테스트 세트를 갖추고 있는 것이 중요합니다. 아두이노 세계에서는 하드웨어 의존성으로 인해 자동화된 테스트가 항상 간단하지 않지만, 순수한 로직 파트에 대해 단위 테스트를 사용하거나 시뮬레이터를 활용할 수 있습니다.

수동 리팩토링의 대안으로는 코드 냄새를 자동으로 식별하고 변경을 제안하는 전용 리팩토링 도구를 사용하는 것이 포함됩니다. 그러나 이러한 도구들은 마이크로콘트롤러 코드의 뉘앙스를 종종 놓치며 아두이노 개발 환경에서 사용할 수 없을 수도 있습니다.

결론적으로, 리팩토링은 코드의 내부 구조를 개선하고 결함 도입의 위험을 균형잡는 예술입니다. 특히 메모리 사용량과 프로세서 시간과 같은 구현 세부 사항에 대해 생각하도록 요구하며, 마이크로콘트롤러의 자원 제한된 특성 때문에 더욱 그렇습니다.

## 참조
마틴 파울러의 저서 *리팩토링: 기존 코드의 설계를 개선하는 방법*을 통해 리팩토링에 대해 더 깊이 탐구할 수 있습니다. 아두이노 특정 관행에 대한 더 자세한 내용은 아두이노 개발 포럼과 커뮤니티를 확인하세요:

- [아두이노 포럼 - 프로그래밍 질문](https://forum.arduino.cc/index.php?board=4.0)
- [리팩토링 구루](https://refactoring.guru/refactoring)

기억하세요, 목표는 미래의 당신과 다른 사람들이 감사할 깨끗하고 이해하기 쉬운 코드입니다. 계속해서 해킹하고, 깔끔하게 유지하세요!
