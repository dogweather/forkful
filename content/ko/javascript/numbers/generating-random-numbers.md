---
date: 2024-01-27 20:34:39.500544-07:00
description: "\uC5B4\uB5BB\uAC8C \uD560\uAE4C: \uC790\uBC14\uC2A4\uD06C\uB9BD\uD2B8\
  \uC5D0\uC11C \uB09C\uC218\uB97C \uC0DD\uC131\uD558\uB294 \uAC00\uC7A5 \uAC04\uB2E8\
  \uD55C \uBC29\uBC95\uC740 `Math.random()`\uC744 \uC0AC\uC6A9\uD558\uB294 \uAC83\uC785\
  \uB2C8\uB2E4. \uC774 \uD568\uC218\uB294 0(\uD3EC\uD568)\uBD80\uD130 1(\uBBF8\uD3EC\
  \uD568)\uC758 \uBC94\uC704 \uB0B4\uC5D0\uC11C \uBD80\uB3D9 \uC18C\uC218\uC810, \uC758\
  \uC0AC \uB09C\uC218\uB97C \uBC18\uD658\uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.785109-06:00'
model: gpt-4-0125-preview
summary: "\uC790\uBC14\uC2A4\uD06C\uB9BD\uD2B8\uC5D0\uC11C \uB09C\uC218\uB97C \uC0DD\
  \uC131\uD558\uB294 \uAC00\uC7A5 \uAC04\uB2E8\uD55C \uBC29\uBC95\uC740 `Math.random()`\uC744\
  \ \uC0AC\uC6A9\uD558\uB294 \uAC83\uC785\uB2C8\uB2E4."
title: "\uB09C\uC218 \uC0DD\uC131"
weight: 12
---

## 어떻게 할까:


### 기본 난수 생성
자바스크립트에서 난수를 생성하는 가장 간단한 방법은 `Math.random()`을 사용하는 것입니다. 이 함수는 0(포함)부터 1(미포함)의 범위 내에서 부동 소수점, 의사 난수를 반환합니다.

```javascript
let randomNumber = Math.random();
console.log(randomNumber);
```

### 범위 내에서 난수 생성
종종 특정 범위 내의 난수 정수가 필요할 것입니다. 이는 `Math.random()`의 출력을 스케일링하고 반올림함으로써 달성할 수 있습니다.

```javascript
function getRandomInt(min, max) {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

console.log(getRandomInt(1, 100));
```

### 암호학적으로 안전한 난수 생성
높은 수준의 무작위성이 요구되는 응용 프로그램(예: 암호화 작업)에는 `crypto.getRandomValues()` 메서드를 사용할 수 있습니다. 이는 `Math.random()`에 의해 생성된 의사 난수와 달리 암호학적 무작위성을 제공합니다.

```javascript
(function generateSecureRandom() {
  let array = new Uint32Array(1);
  window.crypto.getRandomValues(array);
  console.log(array[0]);
})();
```

## 심층 분석
역사적으로 자바스크립트에서의 난수 생성은 `Math.random()` 함수에 전적으로 의존했었습니다. 대부분의 일반적인 사용 사례에 편리하긴 하지만, 그 알고리즘은 메르센 트위스터와 같은 의사난수 생성기(PNRG)의 변형으로, 암호학적 보안을 제공하지 않습니다.

웹 암호화 API의 도입은 보안에 민감한 응용 프로그램에 적합하고 예측하기 어려운 난수를 생성할 수 있는 방법을 제공하는 `crypto.getRandomValues()` 메서드를 가져왔습니다. 이 메서드는 `/dev/random`과 같은 운영 체제의 난수 소스에 접근하여, 암호화 작업에 더욱 강력하고 적합합니다.

수행하는 작업에 적합한 메서드를 선택하는 것이 중요합니다. 단순한 게임, 애니메이션 또는 무작위성의 질이 중요하지 않은 경우라면 `Math.random()`이 충분합니다. 그러나 비밀번호 재설정 토큰이나 암호화 작업과 같은 보안 기능의 경우, 우수한 무작위성 품질 때문에 `crypto.getRandomValues()`가 더 나은 선택입니다.

특히, `Math.random()`은 대부분의 구현에서 알려진 편향을 가지고 있어, 일부 숫자가 다른 숫자보다 더 자주 발생하는 경향이 있습니다. 이 편향은 일반적인 응용 프로그램에서는 미미하고 종종 눈에 띄지 않을 수 있지만, 온라인 도박과 같이 공정성이 중요한 응용 프로그램이나 암호학적 맥락에서 `Math.random()`을 사용하는 것은 부적합합니다.

결론적으로, 자바스크립트 내장 함수는 다양한 필요를 충족시키는 난수 생성을 위한 폭넓은 범위를 커버하지만, 각 메서드의 차이점과 한계를 이해하는 것이 적절한 적용을 위해 필수적입니다.
