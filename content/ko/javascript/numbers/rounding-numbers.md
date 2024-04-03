---
date: 2024-01-26 03:45:33.457728-07:00
description: "\uBC18\uC62C\uB9BC\uC740 \uC22B\uC790\uC758 \uD2B9\uC815 \uC9C0\uC810\
  \ \uC774\uD6C4\uC758 \uC7A1\uC74C\uC744 \uC790\uB974\uB294 \uAC83\uC785\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC815\uBC00\uB3C4\uB97C \uC81C\uC5B4\
  \uD558\uAC70\uB098, \uBA54\uBAA8\uB9AC\uB97C \uAD00\uB9AC\uD558\uAC70\uB098, \uCD9C\
  \uB825\uC744 \uC0AC\uC6A9\uC790 \uCE5C\uD654\uC801\uC73C\uB85C \uB9CC\uB4E4\uAE30\
  \ \uC704\uD574 \uBC18\uC62C\uB9BC\uC744 \uC0AC\uC6A9\uD569\uB2C8\uB2E4\u2014\uC608\
  \uB97C \uB4E4\uC5B4 2.998\uC744 \uAE54\uB054\uD55C 3\uC73C\uB85C \uBCC0\uD658\uD558\
  \uB294 \uAC83\uCC98\uB7FC."
lastmod: '2024-03-13T22:44:55.783609-06:00'
model: gpt-4-0125-preview
summary: "\uBC18\uC62C\uB9BC\uC740 \uC22B\uC790\uC758 \uD2B9\uC815 \uC9C0\uC810 \uC774\
  \uD6C4\uC758 \uC7A1\uC74C\uC744 \uC790\uB974\uB294 \uAC83\uC785\uB2C8\uB2E4."
title: "\uC22B\uC790 \uBC18\uC62C\uB9BC\uD558\uAE30"
weight: 13
---

## 어떻게 하는가:
다음은 JavaScript에서 `Math.round()`, `Math.ceil()`, `Math.floor()`를 사용하여 숫자를 반올림하는 방법입니다:

```javascript
let originalNumber = 2.567;

let roundedDown = Math.floor(originalNumber); // 2
let roundedUp = Math.ceil(originalNumber);    // 3
let rounded = Math.round(originalNumber);     // 3 (.567이 .5보다 크기 때문에)

console.log(roundedDown); // 출력: 2
console.log(roundedUp);   // 출력: 3
console.log(rounded);     // 출력: 3
```

특정 소수점 자리수까지 고정하려면 `toFixed()`를 사용하십시오:

```javascript
let twoDecimals = originalNumber.toFixed(2); // "2.57" (문자열을 반환)

console.log(twoDecimals); // 출력: "2.57"
```

단항 덧셈 또는 `Number()`를 사용하여 문자열을 다시 숫자로 변환합니다:

```javascript
let numberAgain = +twoDecimals; // 2.57

console.log(numberAgain); // 출력: 2.57
```

## 심층 분석
숫자 반올림은 새로운 것이 아닙니다; 숫자가 있는 이래로 있었습니다. JavaScript에서 `Math.round()`는 "반올림" 타이브레이킹을 사용합니다: 소수 부분이 0.5인 경우 가장 가까운 짝수 숫자로 반올림합니다.

더 많은 제어가 필요한 경우 `toFixed()`를 사용할 수 있지만, 문자열을 반환한다는 것을 기억하세요. 숫자로 다시 변환하는 것은 추가 단계일 수 있지만 숫자 유형으로 계속 작업하게 보장합니다.

대안이 있나요? 더 세밀한 제어를 위해 `lodash` 같은 라이브러리는 `_.round(number, [precision=0])`을 제공합니다. 또는, 더 신규의 `Intl.NumberFormat`은 반올림뿐만 아니라 고정밀 포매팅을 제공합니다.

정밀도에 관해서는, JavaScript에서 부동 소수점의 괴상함을 주의하세요. `0.1 + 0.2`가 정확히 `0.3`과 같지 않은 이유는 숫자가 저장되는 방식 때문입니다. 때때로, 이런 부동 소수점 오류를 수정하기 위해 반올림이 필요하게 됩니다.

## 참고할 것
- 모질라의 수학 문서: [MDN 웹 문서](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math)
- `Intl.NumberFormat`을 이용한 금융 반올림: [ECMAScript 국제화 API](https://tc39.es/ecma402/#numberformat-objects)
- `lodash` 반올림: [Lodash 문서](https://lodash.com/docs/4.17.15#round)
