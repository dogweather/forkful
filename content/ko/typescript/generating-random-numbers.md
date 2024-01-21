---
title:                "난수 생성하기"
date:                  2024-01-20T17:50:18.041297-07:00
model:                 gpt-4-1106-preview
simple_title:         "난수 생성하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)

랜덤 숫자 생성하는 것은 예측할 수 없는 숫자를 만드는 작업입니다. 프로그래머들은 게임, 시뮬레이션, 보안 알고리즘 등에서 불확실성을 필요로 할 때 사용합니다.

## How to: (어떻게 하나요?)

TypeScript는 자체적으로 랜덤 숫자를 생성할 수 있는 메서드를 제공합니다. `Math.random()` 함수를 사용하면 됩니다.

```typescript
function getRandomInt(max: number): number {
  return Math.floor(Math.random() * max);
}

console.log(getRandomInt(10)); // 0부터 9까지의 랜덤 정수
console.log(getRandomInt(100)); // 0부터 99까지의 랜덤 정수
```

눈여겨 볼 점은 `Math.random()`이 0 이상 1 미만의 실수를 반환하기 때문에, 정수 범위로 조절하기 위해 `Math.floor`와 최대값을 곱해주는 것입니다.

## Deep Dive (심층 분석)

랜덤 숫자 생성의 역사는 컴퓨터 과학 초기부터 시작되었습니다. 이상적으로, 완벽한 랜덤성은 자연 현상에서만 발생하지만, 컴퓨터는 알고리즘을 통해 "의사 랜덤" 숫자, 즉 예측 불가능하지만 결국에는 알고리즘적으로 생성된 숫자를 제공합니다.

TypeScript/JavaScript에서 `Math.random()` 함수를 사용하면 간단하게 랜덤 숫자를 생성할 수 있으나, 이는 암호학적으로 안전하지 않습니다. 암호학적으로 안전한 랜덤 숫자가 필요한 경우, Web Crypto API의 `crypto.getRandomValues()` 함수를 사용해야 합니다.

다른 언어에서는 다양한 라이브러리와 함수가 제공되며, 사용법 또한 달라질 수 있으니 참고하시기 바랍니다.

## See Also (참조)

- MDN Web Docs: Math.random() - https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random
- MDN Web Docs: Crypto.getRandomValues() - https://developer.mozilla.org/en-US/docs/Web/API/Crypto/getRandomValues
- TypeScript Official Documentation - https://www.typescriptlang.org/docs/