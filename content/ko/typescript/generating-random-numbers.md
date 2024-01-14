---
title:                "TypeScript: 랜덤 숫자 생성하기"
programming_language: "TypeScript"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜

난수를 생성하는 것에 대해 왜 관심있게 될까요? 여러분에게는 한 가지 큰 이유가 있습니다 - 랜덤한 결과를 얻고 싶기 때문입니다! 프로그램에서 난수를 생성하면 보다 다양한 결과를 얻을 수 있습니다. 예를 들어, 게임을 만들 때 매번 같은 방법으로 게임 진행되는 것보다 매번 다른 상황을 만들어 내는 것이 더 재미있을 수 있습니다.

## 어떻게 만들까요?

TypeScript로 난수를 생성하는 방법은 꽤 간단합니다. 아래의 코드를 참고해보세요. 

```TypeScript
// Math.random() 함수를 사용하여 0과 1 사이의 난수 생성
const randomNumber = Math.random();

// 정수로 변환하기 위해 Math.floor() 함수 사용
const randomInteger = Math.floor(randomNumber);

// 범위 내에서 난수 생성하려면 범위를 곱한 후 Math.floor() 함수 사용
// 예를 들어, 0에서 10까지의 정수 중 난수 생성하려면 다음과 같이 작성합니다.
const randomRange = Math.floor(Math.random() * 10);
```

위 코드를 실행하면 각각 다른 난수가 생성됩니다. 예를 들어, 0과 1 사이에서 0.4321이라는 난수를 생성한 경우, `randomNumber` 변수에는 0.4321이 할당되고 `randomInteger` 변수에는 0이 할당됩니다. 그리고 범위를 곱한 후에는 당연히 소수점이 제거되기 때문에 `randomRange` 변수에는 0, 1, 2, ..., 9 중 한 가지 숫자가 할당될 것입니다.

## 딥 다이브

난수 생성에 대해 더 깊이 알아보고 싶은 분들을 위해, TypeScript에서 `Math.random()` 함수가 어떻게 동작하는지 살펴보겠습니다. 

`Math.random()` 함수는 내부적으로 난수를 생성하기 위해 이항(Gaussian) 분포를 사용합니다. 이항 분포는 대부분의 자연적인 현상에서 발생하는 수들과 비슷한 분포를 가지고 있습니다. 이것은 일종의 경험적 규칙으로, 우리 주변에서 많이 볼 수 있는 분포라고 볼 수 있습니다.

이항 분포는 대략적으로 백만 개의 난수를 생성한 뒤, 이들을 합산하여 얻을 수 있는 값을 빈티지(vintage) 나누기에 사용합니다. 이를 통해 더 큰 숫자를 얻을 수 있게됩니다. 그리고 이들 중 어느 하나를 선택해 결과로 반환합니다.

## 또 보기

[타입스크립트 공식 웹사이트 - Math.random()](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-0.html#the-libdomd-ts-file-in---libdom-ts)
[자바스크립트 자습서 - 난수 생성](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Math/random)