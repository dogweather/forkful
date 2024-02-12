---
title:                "연관 배열 사용하기"
aliases: - /ko/typescript/using-associative-arrays.md
date:                  2024-01-30T19:13:26.463799-07:00
model:                 gpt-4-0125-preview
simple_title:         "연관 배열 사용하기"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇이며 왜 사용할까?

연관 배열 또는 TypeScript에서의 객체는 값을 쌍으로 접근하기 위해 문자열(또는 키)을 사용하게 해줍니다. 프로그래머들은 전통적인 배열에 비해 더 동적인 데이터 접근 패턴을 사용하기 위해 이를 사용하며, 숫자 인덱스에 구애받지 않고 데이터의 구조를 짜고 접근하는 유연한 방법을 제공합니다.

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
