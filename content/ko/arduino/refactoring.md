---
title:                "리팩토링"
date:                  2024-01-26T01:17:03.903519-07:00
model:                 gpt-4-0125-preview
simple_title:         "리팩토링"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/refactoring.md"
---

{{< edit_this_page >}}

## 무엇인가 & 왜인가?
리팩토링은 코드의 외부 동작이나 기능을 변경하지 않으면서 코드를 재작업하여 구조와 가독성을 개선하는 과정입니다. 프로그래머들은 코드를 더욱 깨끗하고 이해하기 쉽고 유지보수하기 쉽게 만들기 위해 리팩토링을 합니다. 장기적으로 보면 디버깅과 새로운 기능 추가를 훨씬 덜 골치 아프게 만듭니다.

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
