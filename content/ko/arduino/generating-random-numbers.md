---
title:                "랜덤 숫자 생성하기"
html_title:           "Rust: 랜덤 숫자 생성하기"
simple_title:         "랜덤 숫자 생성하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

난수 생성은 무작위 수를 만드는 과정을 의미합니다. 프로그래머들은 때때로 무작위 결과나 실험을 위해 이를 사용합니다.

## 어떻게 하는가:

Arduino에서 난수를 생성하는 가장 간단한 방법은 `random()` 함수를 사용하는 것입니다.
```Arduino
void setup() {
  Serial.begin(9600);
  randomSeed(analogRead(0));
}

void loop() {
  Serial.println(random(100));
  delay(1000);
}
```
위의 코드는 0에서 99까지의 난수를 매초 생성하고 출력합니다.

## 깊이 들여다 보기:

난수 생성은 프로그래밍의 초기 단계부터 존재했습니다. 이는 게임 시나리오, 실험적 상황, 또는 데이터를 셔플링하는데 사용되었습니다. 대안적으로, `rand()` 함수를 사용할 수도 있지만, Arduino에서는 `random()` 함수가 권장되며, 특히 `randomSeed(analogRead(0))`을 통해 초기화하여 보다 더 무작위성을 높일 수 있습니다.

## 참고:

1. Arduino 공식 난수 문서: https://www.arduino.cc/reference/en/language/functions/random-numbers/random/

2. 더복잡한 난수 생성을 위한 가이드: https://create.arduino.cc/projecthub/electropeak/understanding-random-number-generation-in-arduino-45cea4

3. 난수에 대한 기본적 이해를 돕는 영상: https://www.youtube.com/watch?v=GtOt7EBNEwQ