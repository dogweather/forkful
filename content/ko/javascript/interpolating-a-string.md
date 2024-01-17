---
title:                "문자열 보간"
html_title:           "Javascript: 문자열 보간"
simple_title:         "문자열 보간"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열을 내삽하는 것은 프로그래머가 문자열에 변수를 추가하여 동적인 내용을 생성하고 수정할 수 있도록 하는 것입니다. 이는 반복적인 작업을 피할 수 있고 더 유연한 코드를 작성할 수 있게 해줍니다.

## 방법:

```javascript
// 변수와 함께 문자열 만들기
let name = "John";
console.log(`안녕하세요, ${name}님!`); // 출력: 안녕하세요, John님!

// 조건문과 함께 문자열 만들기
let age = 30;
console.log(`당신의 나이는 ${age >= 18 ? "성인" : "미성년자"}입니다.`); // 출력: 당신의 나이는 성인입니다.
```

## 깊이 파보기:

- 문자열 내삽은 자바스크립트 ES6 버전부터 지원되기 시작했습니다.
- 기존에는 문자열을 연결하는 방식으로 동적인 문자열을 만들었지만, 내삽은 더 간편하고 가독성이 높은 방식입니다.
- 다른 언어에서는 문자열 내삽 대신 문자열 포맷팅을 사용할 수 있습니다.

## 참고:

- [ES6 문자열 내삽](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/template_strings)
- [문자열 포맷팅 vs 내삽](https://medium.com/@vstean/interpolation-vs-string-concatenation-vs-template-literals-202575b9721c)