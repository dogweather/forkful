---
title:                "임의의 숫자 생성"
html_title:           "TypeScript: 임의의 숫자 생성"
simple_title:         "임의의 숫자 생성"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜

랜덤 숫자를 생성하는데 참여하는 이유는 무엇일까요? 우리는 때때로 우리의 프로그램이나 게임에서 무작위 숫자가 필요할 수 있습니다. 예를 들어, 게임에서 주사위를 굴릴 때마다 다른 값을 가진 무작위 숫자가 필요합니다. 이때 우리는 랜덤 숫자를 생성하는 기능이 필요하며, 이를 위해 TypeScript를 사용할 수 있습니다.

## 어떻게

우리는 TypeScript를 사용하여 랜덤 숫자를 생성하는 방법을 알아보겠습니다. 먼저, Math 라이브러리의 random() 함수를 사용하여 0에서 1 사이의 랜덤 실수를 생성할 수 있습니다.

```TypeScript
let randomNumber = Math.random();
console.log(randomNumber); // 예시 출력: 0.37618473679410747
```

또 다른 예시로, 범위를 지정하여 랜덤 정수를 생성하는 방법도 있습니다. 이를 위해 Math 라이브러리의 floor() 함수와 random() 함수를 조합하여 범위 내에서 랜덤 정수를 생성할 수 있습니다.

```TypeScript
let min = 1;
let max = 10;
let randomNumber = Math.floor(Math.random() * (max - min + 1)) + min;
console.log(randomNumber); // 예시 출력: 5
```

## 딥 다이브

랜덤 숫자를 생성하는 방법에 대해 좀 더 깊이 살펴보겠습니다. Math 라이브러리의 random() 함수는 실제로 유사 난수(비결정적인 무작위 값)를 생성합니다. 이는 시드(seed) 값을 입력하지 않았기 때문입니다. 따라서 동일한 코드를 여러 번 실행하면 항상 다른 무작위 값을 얻을 수 있습니다.

반면, 난수를 생성하는 다른 방법으로는 시드 값을 입력하여 결정론적인 난수(항상 동일한 무작위 값을 생성하는 값)를 생성하는 난수 알고리즘을 사용할 수 있습니다. 이를 위해서는 외부 라이브러리를 사용해야 할 수도 있습니다. 예를 들어, seedrandom 라이브러리를 사용하면 시드 값과 함께 무작위 값을 생성할 수 있습니다.

## 관련 링크

- TypeScript 공식 문서: https://www.typescriptlang.org/docs
- Math 라이브러리 문서: https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Math
- seedrandom 라이브러리: https://cdnjs.com/libraries/seedrandom