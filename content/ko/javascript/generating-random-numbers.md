---
title:                "임의의 숫자 생성하기"
html_title:           "Elixir: 임의의 숫자 생성하기"
simple_title:         "임의의 숫자 생성하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
랜덤 숫자 생성이란 정의에 맞게 불규칙한 수를 만드는 것을 의미합니다. 이는 게임, 통계적 시뮬레이션, 보안과 같은 기능에서 의사 난수를 통해 예측할 수 없는 결과를 만들길 원할 때 프로그래머들이 주로 사용합니다.

## 방법:
아래는 JavaScript에서 난수를 생성하는 방법의 예시입니다:

```Javascript
function getRandomInt(max) {
  return Math.floor(Math.random() * Math.floor(max));
}

console.log(getRandomInt(3));
console.log(getRandomInt(1));
console.log(getRandomInt(9));
```
출력 예시:
```Javascript
2
0
7
```
위 코드에서 `Math.random()`은 0과 1사이의 부동소수점, [0, 1) 범위의 새로운 의사 난수를 반환합니다. `Math.floor()`를 사용하여 결과를 소수 아래로 반올림하여 정수를 얻습니다.

## 깊게 알아보기
**역사적 맥락**: JavaScript의 `Math.random()`은 결정론적이며, 완벽하게 무작위 수를 생성하지 못한다는 연구 결과가 있습니다. 이는 컴퓨터가 정교한 계산으로 작동하므로 이전의 숫자에 기반하여 '임의성'을 만들어내야하기 때문입니다.

**대체 방법**: JavaScript로 더 안전한 난수를 생성하기 위한 대안은 표준 라이브러리의 `crypto.getRandomValues()` 함수를 사용하는 것입니다. 이 함수는 보안상 중요한 목적으로 랜덤 값을 요구하는 경우에 효과적입니다.

**구현 세부 정보**: JavaScript의 `Math.random()`은 '0'을 포함하고 '1'을 제외하는 값을 반환합니다. 즉, 반환값은 앞의 예에서와 같이 연속된 범위안의 어떠한 값도 될 수 있습니다.

## 참고:
다음은 주제에 대한 추가 학습을 위한 몇 가지 유용한 링크입니다:
- [MDN (getRandomValues) documentation](https://developer.mozilla.org/ko/docs/Web/API/Crypto/getRandomValues) - 'getRandomValues' 함수에 대한 MDN 문서입니다.
- [JavaScript randomness](https://www.javascript.info/random) - JavaScript에서 난수를 생성하는 다양한 방법을 자세히 설명한 글입니다.
- [Speaking JavaScript](http://speakingjs.com/es5/ch11.html) – JavaScript의 Math.random ()의 동작에 대한 깊은 이해를 제공합니다.