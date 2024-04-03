---
date: 2024-01-27 20:32:53.884522-07:00
description: "\uC5B4\uB5BB\uAC8C: \uC544\uB450\uC774\uB178\uB294 \uB09C\uC218\uB97C\
  \ \uC0DD\uC131\uD558\uAE30 \uC704\uD55C \uAC04\uB2E8\uD55C \uD568\uC218\uB4E4\uC744\
  \ \uC81C\uACF5\uD569\uB2C8\uB2E4: `randomSeed()` \uBC0F `random()`. \uC2DC\uC791\
  \uD558\uAE30 \uC704\uD574, \uD504\uB85C\uADF8\uB7A8\uC774 \uC2E4\uD589\uB420 \uB54C\
  \uB9C8\uB2E4 \uB2E4\uB978 \uC22B\uC790 \uC2DC\uD000\uC2A4\uB97C \uBCF4\uC7A5\uD558\
  \uAE30 \uC704\uD574 \uB09C\uC218 \uC0DD\uC131\uAE30\uB97C \uC2DC\uB4DC\uB85C \uC124\
  \uC815\uD558\uC2ED\uC2DC\uC624. \uC790\uC8FC \uC0AC\uC6A9\uB418\uB294 \uC811\uADFC\
  \uBC95\uC740 \uC5F0\uACB0\uB418\uC9C0 \uC54A\uC740 \uD540\uC5D0\uC11C\u2026"
lastmod: '2024-03-13T22:44:55.602961-06:00'
model: gpt-4-0125-preview
summary: "\uC544\uB450\uC774\uB178\uB294 \uB09C\uC218\uB97C \uC0DD\uC131\uD558\uAE30\
  \ \uC704\uD55C \uAC04\uB2E8\uD55C \uD568\uC218\uB4E4\uC744 \uC81C\uACF5\uD569\uB2C8\
  \uB2E4."
title: "\uB09C\uC218 \uC0DD\uC131"
weight: 12
---

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
