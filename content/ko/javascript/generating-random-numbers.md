---
title:                "랜덤 숫자 생성"
html_title:           "Javascript: 랜덤 숫자 생성"
simple_title:         "랜덤 숫자 생성"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜

랜덤 수를 생성하는 것은 개발자에게 다양한 분야에서 유용합니다. 게임 개발에서는 랜덤 수를 사용하여 다양한 게임 요소를 생성하거나, 테스트를 위해 랜덤한 데이터를 생성할 수 있습니다. 또한 데이터 분석에서 랜덤 수를 생성하여 머신러닝 알고리즘의 성능을 평가할 수도 있습니다.

## 방법

```Javascript
// 1부터 10까지의 랜덤한 정수
let num = Math.floor(Math.random() * 10) + 1;
console.log(num); // 예시 출력: 7
```

JavaScript에서 랜덤한 수를 생성하기 위해서는 `Math.random()` 함수를 활용합니다. 이 함수는 0부터 1사이의 값을 반환하며, 이를 활용하여 원하는 범위의 랜덤한 수를 생성할 수 있습니다. 위의 예시 코드에서는 0부터 9까지의 난수를 생성하고 이에 1을 더하여 1부터 10까지의 난수를 생성하였습니다.

```Javascript
// 1에서 5까지의 랜덤 소수
let floatNum = Math.random() * 5 + 1;
console.log(floatNum.toFixed(2)); // 예시 출력: 3.14
```

정수만이 아닌 소수 형태의 랜덤 수를 생성하려면 `Math.random()`의 반환값에 곱하는 숫자를 조절하여 원하는 범위의 소수를 생성할 수 있습니다. 위의 예시 코드에서는 0부터 4까지의 난수를 생성하고 이에 1을 더하여 1부터 5까지의 난수를 생성한 뒤, `toFixed()` 메소드를 사용하여 소수점 아래 두 자리까지 출력하였습니다.

## 심층 분석

무작위 수의 생성은 의사 난수(Pseudo-random) 알고리즘을 이용하여 이루어집니다. 이는 초기값(seed)를 사용하여 난수의 순서를 결정하는 알고리즘으로, 같은 초기값을 사용하면 항상 같은 순서로 난수가 생성되기 때문에 실제 무작위로 보여지지만 컴퓨터가 계산한 결과입니다. 따라서 초기값을 변경하거나 데이터를 섞는 등의 방법으로 보다 다양한 난수를 생성할 수 있습니다.

## 참고 자료

- [MDN Web Docs: Math.random()](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [W3Schools: JavaScript Random](https://www.w3schools.com/js/js_random.asp)
- [Khan Academy: Algorithmic randomness (의사난수)](https://ko.khanacademy.org/computing/computer-science/cryptography/crypt/v/random-vs-pseudorandom-number-generators)