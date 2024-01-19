---
title:                "문자열 연결하기"
html_title:           "Arduino: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## 무엇이며 왜합니까?

문자열 연결은 두 개 이상의 문자열을 하나로 결합하는 작업을 의미합니다. 프로그래머는 일반적으로 코드의 가독성을 높이기 위해 또는 다양한 데이터 소스에서 정보를 병합하기 위해 문자열을 연결합니다.

## 방법:

TypeScript에서 문자열을 연결하는 방법은 여러 가지가 있습니다. `+` 연산자를 사용한 방법이나 `\` 템플릿 리터럴을 사용한 방법 등입니다.

```TypeScript
let str1 = "안녕, ";
let str2 = "세상";
let result = str1 + str2;
console.log(result); // "안녕, 세상"
```

템플릿 리터럴을 사용한 경우:

```TypeScript
let str1 = "안녕, ";
let str2 = "세상";
let result = `${str1}${str2}`;
console.log(result); // "안녕, 세상"
```

## 깊게 이해하기:

JavaScript의 초기 버전에서는 `+` 연산자만 사용하여 문자열을 연결할 수 있었습니다. 개발자들은 이 방법이 가독성을 저해한다는 문제점을 지적하였고 그 결과, ES6부터는 `\` 템플릿 리터럴이 도입되었습니다. 템플릿 리터럴을 사용하면 코드가 더욱 간결해지고 문자열 내에서 변수를 쉽게 사용할 수 있게 되었습니다.

문자열 연결에 대한 대안으로는 `concat()` 함수가 있습니다. 하지만 이 방법은 `+` 연산자나 템플릿 리터럴에 비해 일반적으로 덜 사용됩니다. 그 이유는 `concat()` 함수를 사용하면 코드가 더 복잡하고 가독성이 낮아지기 때문입니다.

```TypeScript
let str1 = "안녕, ";
let str2 = "세상";
let result = str1.concat(str2);
console.log(result); // "안녕, 세상"
```

## 참고할 만한 자료:

- [MDN Web Docs: 문자열](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String)
- [TypeScript Handbook: 문자열](https://www.typescriptlang.org/docs/handbook/2/strings.html)
- [JavaScript 문자열 연결: + 연산자 vs concat() 함수](https://medium.com/@coderasha/javascript-concat-vs-operator-performance-test-80a61d97df03)