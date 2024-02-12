---
title:                "난수 생성"
aliases: - /ko/arduino/generating-random-numbers.md
date:                  2024-01-27T20:32:53.884522-07:00
model:                 gpt-4-0125-preview
simple_title:         "난수 생성"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 무엇이며 왜?
아두이노 프로젝트에서 난수를 생성하는 것은 디자인에 의해 예측할 수 없는 값들을 생산하는 것을 포함하며, 게임, 시뮬레이션, 보안 시스템과 같은 애플리케이션에 있어서 필수적입니다. 프로그래머들은 이 기술을 사용하여 변동성을 도입하거나 결정적이어서는 안 되는 결정을 내립니다.

## 어떻게:
아두이노는 난수를 생성하기 위한 간단한 함수들을 제공합니다: `randomSeed()` 및 `random()`. 시작하기 위해, 프로그램이 실행될 때마다 다른 숫자 시퀀스를 보장하기 위해 난수 생성기를 시드로 설정하십시오. 자주 사용되는 접근법은 연결되지 않은 핀에서 아날로그 읽기로 시드를 설정하는 것입니다.

```Arduino
void setup() {
  Serial.begin(9600);
  // 난수 시드 초기화
  randomSeed(analogRead(0));
}

void loop() {
  // 0과 99 사이의 난수 생성
  int randomNumber = random(100);
  Serial.println(randomNumber);
  delay(1000); // 출력의 가독성을 위한 1초 지연
}
```

위 프로그램은 `setup()` 함수에서 난수 생성기를 초기화하고 각 루프 반복에서 0과 99 사이의 새로운 숫자를 생성하여 시리얼 모니터로 출력합니다.

샘플 출력:
```
42
17
93
...
```

## 심층 분석
아두이노의 `random()` 함수는 내부적으로 결정적 시퀀스를 따르지만 통계적으로 무작위로 보이는 의사 난수 생성기(PRNG)를 활용합니다. 시퀀스의 초기 값 또는 시드는 그 예측 불가능성에 큰 영향을 미치므로, 시작점으로 다소 무작위 입력을 사용하여 `randomSeed()`을 흔히 사용합니다. 아두이노에 의해 생성된 무작위성은 대부분의 취미 프로젝트에 충분하지만 시간이 지남에 따라 예측가능하기 때문에 고안전성 애플리케이션에 대한 기준을 충족하지 못할 수 있습니다. 암호화 목적의 경우 물리적 과정을 활용하여 진정한 난수를 제공할 수 있는 보다 정교한 알고리즘과 하드웨어 난수 생성기(HRNGs)를 살펴보는 것이 좋습니다.
