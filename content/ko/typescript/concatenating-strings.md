---
title:                "문자열 연결"
html_title:           "TypeScript: 문자열 연결"
simple_title:         "문자열 연결"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜
지금 버전의 TypeScript(이하 TypeScript)를 배우는 이유 중 하나는 문자열을 이어붙이는(concatenating) 것이다. 이는 텍스트를 결합하여 더 큰 문자열을 만들 수 있도록 도와준다.

## 하는 방법
```TypeScript
// 예제 1
let firstName: string = "Jane";
let lastName: string = "Smith";

let fullName: string = firstName + " " + lastName;
console.log(fullName); // Jane Smith

// 예제 2
let greeting: string = "Hello";
let name: string = "John";

let message: string = `${greeting}, ${name}!`;
console.log(message); // Hello, John!
```

## 깊게 들어가보기
문자열 이어붙이기는 JavaScript에서도 매우 일반적인 작업이다. TypeScript에서는 문자열의 데이터 타입을 명시해야 하기 때문에 조금 더 엄격한 방법으로 처리해야 한다. 문자열 이어붙이기에는 두 가지 방법이 있다. 첫 번째는 `+` 연산자를 사용하는 것이고, 두 번째는 템플릿 리터럴(template literal)을 이용하는 것이다. 템플릿 리터럴을 사용하면 변수나 표현식을 쉽게 문자열에 포함시킬 수 있다.

## 더 알아보기
- [TypeScript 공식 문서(한국어)](https://www.typescriptlang.org/ko/docs/)
- [TypeScript 핸드북(한국어)](https://typescript-kr.github.io/pages/basic-types.html)
- [관련 기술 블로그(한국어)](https://www.notion.so/TypeScript-a0438a1e24e9438ba22146c91fd40e1c)

## 참고하기
[See Also](translated into Korean)
- [Understanding String Manipulation in TypeScript](https://blog.bitsrc.io/understanding-string-manipulation-in-typescript-f053cd69ede7)
- [Mastering Strings in TypeScript](https://medium.com/@kellyjanderson/mastering-strings-in-typescript-3b0ca38812a2)
- [Working with Strings in TypeScript](https://www.jenniferbland.com/work-with-strings-typescript/)