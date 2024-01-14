---
title:    "TypeScript: 문자열 연결"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## 왜?

우리는 TypeScript로 프로그래밍을 할 때, 여러 개의 문자열을 하나로 이어붙이는 상황에 자주 직면하게 됩니다. 이를테면, 다양한 변수나 함수 등을 사용하여 동적으로 문자열을 생성하고 이를 출력해야 할 때가 있을 것입니다. 이러한 상황에서 문자열을 이어붙이는 것은 중요한 작업이며, TypeScript에서 이것을 어떻게 할 수 있는지 알아보겠습니다.

## 어떻게?

문자열을 이어붙이는 가장 간단한 방법은 `+` 연산자를 사용하는 것입니다. 다음 예시를 살펴봅시다.

```TypeScript
let name = "홍길동";
console.log("저의 이름은 " + name + "입니다.");
```

위 코드의 출력은 다음과 같습니다.

```
저의 이름은 홍길동입니다.
```

이번에는 Template Literals을 사용하여 문자열을 이어붙여보겠습니다. 다음 예제를 살펴봅시다.

```TypeScript
let age = 27;
console.log(`내 나이는 ${age}살입니다.`);
```

위 코드의 출력은 다음과 같습니다.

```
내 나이는 27살입니다.
```

위의 두 가지 방법 외에도 `String.concat()` 메소드를 사용하여 문자열을 이어붙일 수 있습니다. 이 방법은 두 개의 문자열을 하나의 새로운 문자열로 이어붙여줍니다. 다음 예제를 살펴봅시다.

```TypeScript
let word1 = "프로그래밍";
let word2 = "언어";
console.log(word1.concat(word2));
```

위 코드의 출력은 다음과 같습니다.

```
프로그래밍언어
```

따라서, 큰따옴표 `"` 또는 백틱 ``` ` ```으로 감싸진 문자열 안에 변수나 다른 문자열을 삽입하여 문자열을 이어붙일 수 있습니다.

## 딥 다이브

TypeScript에서 문자열을 이어붙이는 방법은 간단하지만, 실제로는 내부적으로 많은 일이 벌어집니다. 예를 들어, Template Literals의 경우 백틱 ``` ` ```으로 감싸진 문자열 안에서 `${}`으로 감싼 부분이 해당 변수의 값으로 대체되는 과정이 있습니다. 이러한 일련의 과정을 통해 우리는 원하는 문자열을 동적으로 생성할 수 있습니다.

## 무엇을 참고하면 좋을까요?

- [TypeScript 문자열 이어붙이기 - w3schools.com](https://www.w3schools.com/jsref/jsref_concat_string.asp)
- [TypeScript 문자열 이어붙이기 - TypeScript 공식 문서](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)