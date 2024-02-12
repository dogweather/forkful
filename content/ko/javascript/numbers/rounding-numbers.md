---
title:                "숫자 반올림하기"
date:                  2024-01-26T03:45:33.457728-07:00
model:                 gpt-4-0125-preview
simple_title:         "숫자 반올림하기"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/rounding-numbers.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 필요한가?
반올림은 숫자의 특정 지점 이후의 잡음을 자르는 것입니다. 프로그래머들은 정밀도를 제어하거나, 메모리를 관리하거나, 출력을 사용자 친화적으로 만들기 위해 반올림을 사용합니다—예를 들어 2.998을 깔끔한 3으로 변환하는 것처럼.

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
