---
date: 2024-01-20 17:43:08.692935-07:00
description: "\uD2B9\uC815 \uD328\uD134\uC5D0 \uB9DE\uB294 \uBB38\uC790\uB97C \uC0AD\
  \uC81C\uD558\uB294 \uAC83\uC740 \uBB38\uC790\uC5F4 \uB0B4 \uBD88\uD544\uC694\uD55C\
  \ \uBD80\uBD84\uC744 \uC81C\uAC70\uD558\uB294 \uACFC\uC815\uC785\uB2C8\uB2E4. \uD504\
  \uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uCF54\uB4DC\uB97C \uB354 \uAE54\uB054\uD558\
  \uAC8C \uC720\uC9C0\uD558\uACE0, \uC6D0\uD558\uB294 \uB370\uC774\uD130 \uD615\uC2DD\
  \uC73C\uB85C \uC815\uBCF4\uB97C \uC815\uC81C\uD558\uAE30 \uC704\uD574 \uC774 \uC791\
  \uC5C5\uC744 \uC218\uD589\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.769281-06:00'
model: gpt-4-1106-preview
summary: "\uD2B9\uC815 \uD328\uD134\uC5D0 \uB9DE\uB294 \uBB38\uC790\uB97C \uC0AD\uC81C\
  \uD558\uB294 \uAC83\uC740 \uBB38\uC790\uC5F4 \uB0B4 \uBD88\uD544\uC694\uD55C \uBD80\
  \uBD84\uC744 \uC81C\uAC70\uD558\uB294 \uACFC\uC815\uC785\uB2C8\uB2E4."
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
