---
title:    "Javascript: 임의의 숫자 생성하기"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

#왜 정수 생성에 관여해야 하는가

우리는 일상에서 많은 수의 난수를 사용합니다. 웹 사이트에서 새로운 패스워드를 생성하거나 무작위로 주어진 예측 번호를 만들기 위해 로또를 구매할 때, 우리는 난수를 사용합니다. 이러한 난수들을 JavaScript로 만들기 위해서는 왜 하는지에 대해 알아야 합니다.

#어떻게 생성하는가

JavaScript에서 무작위로 정수를 생성하는 것은 매우 간단합니다. `Math.random()` 함수는 0 이상 1 미만의 임의의 부동 소수점 숫자를 반환합니다. 이 숫자를 정수로 변환하면 우리가 원하는 난수를 얻을 수 있습니다.

```Javascript
// 1부터 10까지의 난수 생성
let randomNumber = Math.floor(Math.random() * 10) + 1;
console.log(randomNumber); // 출력 예시: 5
```

주의해야 할 점은 `Math.random()`이 철저히 무작위가 아니라는 것입니다. 그래서 악의적으로 보안을 통제할 수 있는 분야에서는 사용하지 않는 것이 좋습니다.

#더 깊게 들어가기

더 많은 난수 관련 함수를 사용하고 싶다면 자바스크립트의 `crypto` 라이브러리를 살펴보세요. 이 라이브러리는 보안 관련 난수 생성과 같은 더 많은 기능을 제공합니다. 또한 `seedrandom`과 같은 서드파티 라이브러리도 좋은 선택일 수 있습니다.

#더 알아보기

[JavaScript에서 난수 생성하는 방법](https://www.freecodecamp.org/news/how-to-generate-random-number-in-javascript/)

[Math.random() 함수의 공식 문서](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)

[crypto 라이브러리의 공식 문서](https://nodejs.org/api/crypto.html)

[seedrandom 라이브러리의 GitHub 페이지](https://github.com/davidbau/seedrandom)