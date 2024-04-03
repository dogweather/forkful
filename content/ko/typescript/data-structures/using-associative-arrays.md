---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:13:26.463799-07:00
description: "\uC0AC\uC6A9 \uBC29\uBC95: TypeScript\uC5D0\uC11C \uC5F0\uAD00 \uBC30\
  \uC5F4\uC744 \uC0DD\uC131\uD558\uACE0 \uC0AC\uC6A9\uD558\uB294 \uAC83\uC740 \uAC04\
  \uB2E8\uD569\uB2C8\uB2E4. \uAE30\uBCF8\uC801\uC778 \uC2E4\uD589 \uBC29\uBC95\uC740\
  \ \uB2E4\uC74C\uACFC \uAC19\uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.843060-06:00'
model: gpt-4-0125-preview
summary: "TypeScript\uC5D0\uC11C \uC5F0\uAD00 \uBC30\uC5F4\uC744 \uC0DD\uC131\uD558\
  \uACE0 \uC0AC\uC6A9\uD558\uB294 \uAC83\uC740 \uAC04\uB2E8\uD569\uB2C8\uB2E4."
title: "\uC5F0\uAD00 \uBC30\uC5F4 \uC0AC\uC6A9\uD558\uAE30"
weight: 15
---

## 사용 방법:
TypeScript에서 연관 배열을 생성하고 사용하는 것은 간단합니다. 기본적인 실행 방법은 다음과 같습니다:

```TypeScript
// 연관 배열 선언하기
let user: { [key: string]: string } = {};

// 데이터 추가하기
user["name"] = "Jane Doe";
user["email"] = "jane@example.com";

console.log(user);
```

출력:

```TypeScript
{ name: 'Jane Doe', email: 'jane@example.com' }
```

키-값 쌍을 반복하는 것 또한 쉽습니다:

```TypeScript
for (let key in user) {
    console.log(key + ": " + user[key]);
}
```

출력:

```TypeScript
name: Jane Doe
email: jane@example.com
```

그리고 만약 여러 가지 데이터 유형을 다루고 있다면, TypeScript의 타입 시스템이 유용합니다:

```TypeScript
let mixedTypes: { [key: string]: string | number } = {};
mixedTypes["name"] = "John Doe";
mixedTypes["age"] = 30;

console.log(mixedTypes);
```

출력:

```TypeScript
{ name: 'John Doe', age: 30 }
```

## 심층 탐구
TypeScript에서 우리가 연관 배열로 언급하는 것은 본질적으로 객체입니다. 역사적으로 PHP와 같은 언어에서 연관 배열은 기본 타입이지만, JavaScript(그리고 TypeScript를 포함해서)는 이 목적을 위해 객체를 사용합니다. 이 접근법은 강점이자 한계입니다. 객체는 문자열을 값에 연관짓기 위한 매우 동적인 구조를 제공하지만, 전통적인 의미에서 '배열'로 사용될 의도는 아닙니다. 예를 들어, 이러한 객체에 대해서는 `push`나 `pop` 같은 배열 메소드를 직접 사용할 수 없습니다.

키-값 쌍의 순서가 있는 컬렉션을 배열 같은 연산과 함께 필요로 하는 경우, TypeScript(및 현대 JavaScript)는 `Map` 객체를 제공합니다:

```TypeScript
let userMap = new Map<string, string>();
userMap.set("name", "Jane Doe");
userMap.set("email", "jane@example.com");

userMap.forEach((value, key) => {
    console.log(key + ": " + value);
});
```

TypeScript의 타입 시스템과 ES6 기능인 `Map`과 같은 강력한 대안들이 제공되지만, 객체 리터럴이 더 효율적인 시나리오에서나 JSON 데이터 구조를 다룰 때 연관 배열로서 객체를 사용하는 방법을 이해하는 것이 유용합니다. 일은 올바른 도구를 선택하는 것에 관한 것입니다.
