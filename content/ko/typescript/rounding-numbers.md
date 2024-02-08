---
title:                "숫자 반올림하기"
aliases:
- ko/typescript/rounding-numbers.md
date:                  2024-01-26T03:47:37.251219-07:00
model:                 gpt-4-0125-preview
simple_title:         "숫자 반올림하기"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/rounding-numbers.md"
---

{{< edit_this_page >}}

## 무엇이며 왜인가?
숫자 반올림은 특정 정밀도까지 숫자를 줄이는 것입니다. 프로그래머들은 이를 수치 출력을 제어하거나, 가독성을 위해, 혹은 부동 소수점 결과를 낸 연산 후 특정 정밀도가 요구될 때 사용합니다.

## 방법:
TypeScript에서 반올림은 여러 방법을 사용하여 수행할 수 있습니다. 간단한 실행 방법은 다음과 같습니다:

```typescript
// Math.round는 가장 가까운 정수로 반올림합니다.
console.log(Math.round(1.5)); // 출력: 2

// Math.ceil은 가장 가까운 정수로 올림합니다.
console.log(Math.ceil(1.1)); // 출력: 2

// Math.floor는 가장 가까운 정수로 내림합니다.
console.log(Math.floor(1.8)); // 출력: 1

// toFixed는 고정된 소수점 자리수로 반올림합니다.
let num = 1.23456;
console.log(num.toFixed(2)); // 출력: "1.23"
// 주의: toFixed는 문자열을 반환합니다! 필요한 경우 parseFloat을 사용하여 다시 변환하세요.
console.log(parseFloat(num.toFixed(2))); // 출력: 1.23
```

## 심층 탐구
예전에는 초기 컴퓨터의 제한된 공간과 정밀도 문제 때문에 반올림이 필수였습니다. 오늘날, 부동 소수점 연산은 이진수로 숫자가 저장되는 방식 때문에 기묘한 결과를 낳을 수 있습니다. 반올림 대안으로는 내림, 올림, 및 소수점 이하를 반올림하지 않고 잘라내는 trunc가 있습니다.

내부 작동이 주목할 만합니다: `Math.round`는 "round half up" (일명 "상업적 반올림")을 따르며, `Math.floor`와 `Math.ceil`은 간단명료합니다. `toFixed`는 문자열을 반환하고 "round half to even" (일명 "은행가의 반올림")을 사용하여 반올림하기 때문에 예상치 못한 결과를 초래할 수 있으며, 특히 동일한 숫자를 여러 번 반올림할 때 편향을 줄이는 데 유용합니다.

## 참고하기
- [MDN - Math.round()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/round)
- [MDN - Math.ceil()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/ceil)
- [MDN - Math.floor()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/floor)
- [MDN - toFixed()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toFixed)
- [IEEE 부동 소수점 산술 표준 (IEEE 754)](https://ieeexplore.ieee.org/document/4610935)
