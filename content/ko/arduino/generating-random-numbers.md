---
title:                "난수 생성하기"
date:                  2024-01-20T17:48:21.097005-07:00
model:                 gpt-4-1106-preview
simple_title:         "난수 생성하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜 필요한가?)

랜덤 숫자 생성은 예측 불가능한 숫자를 만드는 과정입니다. 이는 게임, 시뮬레이션, 보안 시스템 등에서 요소의 불확실성을 부여하기 위해 사용됩니다.

## How to: (어떻게 할까?)

```Arduino
void setup() {
  Serial.begin(9600);          // 시리얼 통신 시작
  randomSeed(analogRead(0));   // 랜덤 시드 초기화
}

void loop() {
  int randomNumber = random(1, 100); // 1부터 99까지 랜덤 숫자 생성
  Serial.println(randomNumber);       // 시리얼 모니터에 출력
  delay(1000);                        // 1초 대기
}
```

예시 출력:
```
42
76
5
...
```

## Deep Dive (심층 분석)

랜덤 숫자 생성은 19세기 중반 이후 통계와 컴퓨터 과학에서 중요한 부분이 되었습니다. Arduino는 본질적으로 진정한 랜덤이 아닌 의사 난수(pseudorandom)를 생성합니다. 이는 일정한 알고리즘을 이용해 생성되므로, `randomSeed()` 함수로 시드 값을 주입해 시작점을 변화시킵니다. 대안으로, 외부 엔트로피 소스(예: `analogRead(0)`의 잡음)를 활용하여 예측 불가능성을 향상시킬 수 있습니다.

## See Also (관련 링크)

- Arduino 공식 문서의 Random 함수: [Arduino Reference](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- 난수 생성기에 대한 이해: [Random Number Generation Wikipedia](https://en.wikipedia.org/wiki/Random_number_generation)