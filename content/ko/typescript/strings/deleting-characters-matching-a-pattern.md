---
date: 2024-01-20 17:43:47.769775-07:00
description: "How to: (\uBC29\uBC95) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:54.831504-06:00'
model: gpt-4-1106-preview
summary: .
title: "\uD328\uD134\uC5D0 \uC77C\uCE58\uD558\uB294 \uBB38\uC790 \uC0AD\uC81C"
weight: 5
---

## How to: (방법)
```TypeScript
function deleteMatchingChars(str: string, pattern: RegExp): string {
  return str.replace(pattern, '');
}

// Example usage:
const originalString = 'Hello, World! 123';
const pattern = /[0-9]/g; // 숫자를 찾아라

const cleanedString = deleteMatchingChars(originalString, pattern);
console.log(cleanedString); // "Hello, World! "
```

## Deep Dive (심층 분석)
문자 삭제는 문자열 내용을 조작할 때 기본적인 동작입니다. 다양한 프로그래밍 언어에서 상이한 구현과 함께 제공됩니다. 자바스크립트와 타입스크립트에서는 `String.prototype.replace` 함수를 사용하여 이 작업을 수행합니다. 확장자 `g`가 있는 정규 표현식을 패턴으로 사용하면 문자열에서 해당 패턴과 일치하는 모든 인스턴스를 찾아 삭제할 수 있습니다. 

`replace` 함수 외에도, 때로는 다른 메쏘드나 라이브러리가 더 적합할 수 있습니다. 예를 들어 Lodash와 같은 유틸리티 라이브러리는 `_.remove`와 같은 특화된 함수를 제공합니다. 하지만 타입스크립트에서 `replace`를 사용하는 것이 일반적으로 가장 간단하며 효율적입니다.

패턴 매칭 삭제 방식은 컴퓨터 과학에서 문자열 처리와 언어 분석의 중요한 부분으로, 이는 1950년대부터 사용되었습니다.

## See Also (참고자료)
- MDN 웹 문서: [String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- 정규 표현식: [RegExp](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp)
