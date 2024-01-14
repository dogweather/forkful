---
title:                "TypeScript: 스트링 연결하기"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜
문자열을 연결하는 과정에 참여하는 이유는 문자열을 조합해 새로운 문자열을 생성하는 직관적이고 간단한 방법이기 때문입니다.

## 하는 방법
우리가 TypeScript를 사용할 때 문자열을 연결하는 가장 일반적인 방법은 `+` 연산자를 사용하는 것입니다. 하지만 여러분은 `+` 연산자를 사용하지 않고도 해당 작업을 수행할 수 있습니다.

```TypeScript
// 예시 1
let str1 = "안녕하세요";
let str2 = "반가워요";
let result = str1 + str2;
console.log(result); // 안녕하세요반가워요

// 예시 2
let str1 = "안녕하세요";
let str2 = "반가워요";
let result = `${str1} ${str2}`;
console.log(result); // 안녕하세요 반가워요
```

또한, 여러 개의 문자열을 연결하는 데에는 `concat()` 메서드를 사용할 수도 있습니다.

```TypeScript
// 예시 1
let str1 = "안녕하세요";
let str2 = "반가워요";
let str3 = "저는 TypeScript를 공부하고 있어요";
let result = str1.concat(str2, str3);
console.log(result); // 안녕하세요반가워요저는 TypeScript를 공부하고 있어요

// 예시 2
let str = "안녕하세요";
let result = str.concat("반가워요", "저는 TypeScript를 공부하고 있어요");
console.log(result); // 안녕하세요반가워요저는 TypeScript를 공부하고 있어요
```

## 더 들어가보기
문자열을 연결하는 두 가지 방법(`+` 연산자, `concat()` 메서드) 모두 새로운 문자열을 만드는 것이 아니라 기존의 문자열을 변형시키는 것입니다. 따라서, 메모리를 절약하기 위해서는 새로운 문자열을 생성하지 않고 기존의 문자열을 이용하는 것이 좋습니다. 또한, `+` 연산자는 작업이 많아질수록 성능이 떨어질 수 있으므로, 가능하다면 템플릿 리터럴을 사용하는 것이 좋습니다.

## 참고자료
- [MDN Web Docs: String concatenation](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- [MDN Web Docs: Template literals](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Template_literals)