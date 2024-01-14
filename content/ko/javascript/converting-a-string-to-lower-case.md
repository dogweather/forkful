---
title:    "Javascript: 문자열 소문자로 변환하기"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

# 왜?

문자열을 소문자로 변환하는 일은 매우 일반적인 작업입니다. 이를 왜해야 하는지 궁금할 수 있습니다. 일반적으로, 소문자 문자열은 비교하기 쉽고, 일관성 있게 변환하기 쉽고, 용량이 작아서 문자열을 다루는 프로그래밍에서 매우 유용합니다.

# 어떻게?

```Javascript
// 예제 1: String.toLowerCase() 메소드를 이용한 간단한 문자열 소문자 변환
const str1 = "HELLO WORLD";
console.log(str1.toLowerCase()); // output: hello world

// 예제 2: for 루프를 이용한 문자열 소문자 변환
const str2 = "JaVaScRiPt";
let result = "";
for (let i = 0; i < str2.length; i++) {
  result += str2[i].toLowerCase();
}
console.log(result); // output: javascript
```

# 더 깊이 들어가보기

문자열을 소문자로 변환하는 방법은 여러 가지가 있습니다. 위 예제들은 간단한 방법이지만, 실제로는 이 밖에도 다양한 방식이 존재합니다. 예를 들어, 정규식을 이용한 변환 방법도 있습니다.

또한, 문자열을 소문자로 변환하는 과정에서 유니코드에 대한 이해도 중요합니다. 언어마다 대문자와 소문자의 구분 방식이 다르기 때문에, 유니코드의 문자 분류를 이해하는 것이 더 나은 결과를 얻는 데에 도움이 될 수 있습니다.

# 관련 자료

[Javascript String 공식 문서](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String)

[유니코드 문자 분류](https://unicode.org/charts/charindex.html)