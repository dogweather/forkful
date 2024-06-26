---
date: 2024-01-20 17:43:08.692935-07:00
description: "How to: (\uBC29\uBC95) \uC774 \uCF54\uB4DC\uB294 \uC790\uBC14\uC2A4\uD06C\
  \uB9BD\uD2B8\uC758 `.replace()` \uBA54\uC11C\uB4DC\uC640 \uC815\uADDC \uD45C\uD604\
  \uC2DD\uC744 \uC0AC\uC6A9\uD558\uC5EC \uBB38\uC790\uB97C \uC0AD\uC81C\uD569\uB2C8\
  \uB2E4. `/g` \uD50C\uB798\uADF8\uB294 \uC804\uC5ED \uAC80\uC0C9\uC744 \uC758\uBBF8\
  \uD558\uBA70, \uBB38\uC790\uC5F4 \uC804\uCCB4\uC5D0\uC11C \uD574\uB2F9 \uD328\uD134\
  \uC744 \uCC3E\uC544 \uC81C\uAC70\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.376568-06:00'
model: gpt-4-1106-preview
summary: "(\uBC29\uBC95) \uC774 \uCF54\uB4DC\uB294 \uC790\uBC14\uC2A4\uD06C\uB9BD\uD2B8\
  \uC758 `.replace()` \uBA54\uC11C\uB4DC\uC640 \uC815\uADDC \uD45C\uD604\uC2DD\uC744\
  \ \uC0AC\uC6A9\uD558\uC5EC \uBB38\uC790\uB97C \uC0AD\uC81C\uD569\uB2C8\uB2E4."
title: "\uD328\uD134\uC5D0 \uC77C\uCE58\uD558\uB294 \uBB38\uC790 \uC0AD\uC81C"
weight: 5
---

## How to: (방법)
```javascript
// 정규 표현식을 사용하여 문자 삭제하기
let text = "123-45-6789";
let newText = text.replace(/\-/g, '');
console.log(newText); // "123456789"

// 문자 집합을 사용하여 여러 문자 삭제하기
let sample = "Hello World! 123";
let cleanSample = sample.replace(/[0-9!]/g, '');
console.log(cleanSample); // "Hello World "
```
이 코드는 자바스크립트의 `.replace()` 메서드와 정규 표현식을 사용하여 문자를 삭제합니다. `/g` 플래그는 전역 검색을 의미하며, 문자열 전체에서 해당 패턴을 찾아 제거합니다.

## Deep Dive (깊은 탐구)
삭제는 문자열 처리에서 중요한 부분입니다. 과거에도 정규 표현식은 문자열을 조작하는 데 있어 강력한 도구였습니다. 자바스크립트에서는 `.replaceAll()`과 같은 새로운 메서드가 도입되어 비슷한 작업을 수행하지만, 정규 표현식은 더 복잡한 패턴 매칭이 필요할 때 여전히 유용합니다.

`.replace()`가 한 번에 하나의 매칭만 처리할 때, `/g` 플래그를 사용하면 모든 매칭을 처리할 수 있습니다. `.replaceAll()`을 사용하면 `/g` 플래그 없이도 모든 매칭을 대체할 수 있기 때문에 코드를 조금 더 간단하게 만들 수 있습니다. 단, `.replaceAll()`은 ES2021에서 도입되었으므로 이전 버전과의 호환성이 필요한 경우 `.replace()`와 정규 표현식을 사용해야 합니다.

알고리즘 효율성 측면에서 볼 때, 정규 표현식 작업은 단순 문자열 메서드 호출보다 더 많은 계산을 요구할 수 있습니다. 이는 복잡한 패턴을 컴파일하고 찾아야 하기 때문입니다. 때문에 패턴이 간단하고 고정된 경우, `.split()`과 `.join()`을 조합한 방식으로 대체할 수도 있습니다.

```javascript
// .split()과 .join()을 사용하여 문자 삭제하기
let simpleText = "abc123";
let removedDigits = simpleText.split(/[0-9]/).join('');
console.log(removedDigits); // "abc"
```

이 방법은 정규 표현식 대신 간단한 문자열 조작을 사용하며, 성능이 더 중요한 상황에서 유용할 수 있습니다.

## See Also (함께 보기)
- MDN Web Docs의 [String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- MDN Web Docs의 [RegExp](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- 자바스크립트 [String.prototype.replaceAll()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replaceAll) 메서드
