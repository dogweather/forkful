---
title:                "난수 생성하기"
date:                  2024-01-20T17:49:44.184514-07:00
model:                 gpt-4-1106-preview
simple_title:         "난수 생성하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
랜덤 숫자를 생성하는 것은 예측 불가능한 숫자를 만드는 프로세스입니다. 프로그래머들은 게임, 시뮬레이션, 보안 시스템, 그리고 테스트 데이터를 만들 때 이를 활용합니다.

## How to: (어떻게 해?)
```javascript
// 기본적인 랜덤 숫자 생성
let randomNum = Math.random(); // 0과 1 사이의 랜덤한 숫자 반환
console.log(randomNum);

// 정수 범위 내에서 랜덤 숫자 생성
let min = 1;
let max = 100;
let randomInt = Math.floor(Math.random() * (max - min + 1)) + min; // 1에서 100 사이의 랜덤한 정수 반환
console.log(randomInt);
```

Sample Output:
```
0.8273629810924768
42
```

## Deep Dive (심층 분석)
프로세서들이 진짜 '랜덤성'을 생성할 수 없기 때문에 JavaScript의 `Math.random()` 함수는 사실적인 확률 분포를 모방하는 난수 생성기를 사용합니다. 이는 의사 랜덤(pseudorandom)이라고 불립니다. 한때는 많은 난수 생성 알고리즘이 있었지만, 현재 대부분의 JavaScript 엔진은 고품질의 난수를 생성하기 위해 Xorshift128+ 같은 첨단 알고리즘을 사용합니다.

더욱 강력한 보안이 필요할 때는 `crypto.getRandomValues()` 메소드를 사용하여 암호학적으로 안전한 난수를 생성할 수 있습니다. 이는 예측이 거의 불가능하며 주로 암호화 작업에 사용됩니다.

```javascript
// 암호학적으로 안전한 난수 생성
let array = new Uint32Array(1);
window.crypto.getRandomValues(array);
console.log(array[0]); // 큰 범위의 정수 난수 반환
```

## See Also (참고할만한 내용)
- MDN Web Docs on `Math.random()`: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random
- MDN Web Docs on `crypto.getRandomValues()`: https://developer.mozilla.org/en-US/docs/Web/API/Crypto/getRandomValues
- Randomness in cryptography: https://en.wikipedia.org/wiki/Randomness
- Xorshift: https://en.wikipedia.org/wiki/Xorshift