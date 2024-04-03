---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:54:12.689194-07:00
description: "\uBC29\uBC95: Google Apps Script\uC5D0\uC11C\uB294 JavaScript\uC640\
  \ \uC720\uC0AC\uD558\uAC8C `Math.random()` \uD568\uC218\uB97C \uC0AC\uC6A9\uD558\
  \uC5EC \uBB34\uC791\uC704 \uC22B\uC790\uB97C \uC0DD\uC131\uD560 \uC218 \uC788\uC2B5\
  \uB2C8\uB2E4. \uC774 \uD568\uC218\uB294 0(\uD3EC\uD568)\uC5D0\uC11C 1(\uBD88\uD3EC\
  \uD568) \uBC94\uC704\uC758 \uBD80\uB3D9\uC18C\uC218\uC810, \uC720\uC0AC \uBB34\uC791\
  \uC704 \uC218\uB97C \uBC18\uD658\uD569\uB2C8\uB2E4. \uD2B9\uC815 \uBC94\uC704 \uB0B4\
  \uC758 \uC815\uC218\uB97C \uC0DD\uC131\uD558\uB294\u2026"
lastmod: '2024-03-13T22:44:54.522022-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script\uC5D0\uC11C\uB294 JavaScript\uC640 \uC720\uC0AC\uD558\
  \uAC8C `Math.random()` \uD568\uC218\uB97C \uC0AC\uC6A9\uD558\uC5EC \uBB34\uC791\uC704\
  \ \uC22B\uC790\uB97C \uC0DD\uC131\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uB09C\uC218 \uC0DD\uC131\uD558\uAE30"
weight: 12
---

## 방법:
Google Apps Script에서는 JavaScript와 유사하게 `Math.random()` 함수를 사용하여 무작위 숫자를 생성할 수 있습니다. 이 함수는 0(포함)에서 1(불포함) 범위의 부동소수점, 유사 무작위 수를 반환합니다. 특정 범위 내의 정수를 생성하는 것과 같은 다양한 사용 사례에 맞춤형 숫자를 제공하기 위해서는 추가적인 계산이 필요할 수 있습니다.

### 기본 무작위 숫자 생성하기
간단한 무작위 숫자를 생성하여 콘솔에 로깅하려면:

```javascript
function generateRandomNumber() {
  var randomNumber = Math.random();
  Logger.log(randomNumber);
}
```
*샘플 출력:* `0.1234567890123456`

### 특정 범위 내의 정수 생성하기
두 값(`최소`와 `최대`) 사이의 무작위 정수를 생성하려면, 두 값 모두 포함:

```javascript
function getRandomInt(min, max) {
  min = Math.ceil(min);
  max = Math.floor(max);
  var randomNumber = Math.floor(Math.random() * (max - min + 1)) + min;
  Logger.log(randomNumber);
  return randomNumber;
}

// 예제:
getRandomInt(1, 10);
```
*샘플 출력*: `7`

`Math.ceil()` 함수는 최소값을 올림하여 정수로 만들고, `Math.floor()` 함수는 최대값을 내림하여 정수로 만들어, 지정된 범위 내에서 무작위 숫자가 생성되도록 합니다.

## 심도 있게 살펴보기
Google Apps Script에서, 실제로 대부분의 프로그래밍 언어에서도, 무작위 숫자를 생성하는 메커니즘은 유사 무작위 수 생성기(PRNG)를 이용합니다. 이 기술은 결정론적이며 시드라고 알려진 초기값에 의존하여 랜덤한 것처럼 보이는 수열을 생성합니다. 많은 응용 프로그램에 충분하지만, 높은 보안 또는 진정한 무작위성이 필요한 암호학적 응용 프로그램과 같은 경우에는 유사 무작위 수가 적절하지 않을 수 있음을 기억하는 것이 중요합니다.

진정한 무작위성은 하드웨어 무작위 수 생성기 또는 자연 현상에서 무작위성을 생성하는 서비스를 통해 달성할 수 있습니다. 하지만, Google Apps Script에서 대부분의 일상적인 스크립팅 요구에는 `Math.random()`이 충분합니다.

역사적으로, 보다 효과적인 무작위 수 생성 기술을 개발하기 위한 탐구는 머신 트위스터과 선형 합동 생성기(LCG)와 같은 다양한 알고리즘의 개발로 이어졌습니다. 그러나, Google Apps Script에서의 높은 추상화 수준을 감안할 때 대부분의 사용자는 이러한 알고리즘을 직접 구현할 필요가 없지만, 스크립트에서 무작위 수 생성의 중요성과 한계를 이해하는데 도움이 될 수 있습니다.
