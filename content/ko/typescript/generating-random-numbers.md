---
title:                "랜덤 숫자 생성"
html_title:           "TypeScript: 랜덤 숫자 생성"
simple_title:         "랜덤 숫자 생성"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

# 무엇 & 왜?

임의의 숫자를 생성하는 것은 프로그래밍에서 중요한 부분입니다. 프로그래머들은 다양한 목적에 따라 무작위 수를 생성합니다. 예를 들어 무작위로 숫자를 생성하여 게임의 난이도를 조절하거나, 암호화를 위해 암호키를 생성하는 등의 목적이 있습니다.

## 방법:

TypeScript에서 임의의 숫자를 생성하는 가장 간단한 방법은 Math.random() 함수를 사용하는 것입니다. 이 함수는 0과 1 사이의 임의의 소수를 반환합니다.

```TypeScript
let randomNumber = Math.random();
console.log(randomNumber); // 예상 출력: 0.7823658948585
```

## 깊이 알아보기:

실제로 임의의 숫자를 생성하기 위해서는 더 복잡한 방법이 필요합니다. Math.random() 함수는 사실은 무작위 수를 생성하는 것이 아니라 랜덤한 결과를 예측할 수 있는 수열을 따르는 것입니다. 따라서 보다 더 신뢰성있는 결과를 얻기 위해선 Math.random() 함수보다 더 많은 조작이 필요합니다.

대안으로, 우리는 더 신뢰성있는 무작위 수 생성기를 사용할 수 있습니다. 예를 들어 Random.org은 연구용으로도 사용 가능한 무작위 수를 생성하는 무료 웹 서비스입니다.

무작위 수를 생성하는데 이 외에도 많은 방법들이 있지만, 정확하고 안전하게 임의의 숫자를 생성하는 것은 어려운 일입니다.

## 더 알아보기:

- [MDN web docs – Math.random()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Random.org](https://www.random.org/)