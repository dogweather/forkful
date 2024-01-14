---
title:                "TypeScript: 텍스트 검색 및 대체"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜

텍스트 검색 및 대체를 사용하는 이유는 프로그래밍에서 일반적인 작업 중 하나입니다. 특정 텍스트를 찾아서 다른 텍스트로 대체함으로써 손쉽게 코드를 수정할 수 있습니다.

## 방법

```TypeScript
// "Hello, world!"라는 문자열에서 "world"를 "universe"로 바꾸는 예제 코드
let string = "Hello, world!";
string = string.replace("world", "universe");
console.log(string); // 출력 결과: "Hello, universe!"
```

위의 예제 코드처럼 `replace()` 메소드를 사용하면 특정 문자열을 다른 문자열로 대체할 수 있습니다. 이 외에도 정규표현식을 사용하여 패턴에 일치하는 모든 문자열을 대체할 수도 있습니다. 

```TypeScript
// "apple"이라는 문자열이 있는지 확인 후, 있다면 "banana"로 대체하는 예제 코드
let fruits = ["apple", "orange", "grape"];
if(fruits.includes("apple")){
  fruits = fruits.map(fruit => fruit === "apple" ? "banana" : fruit);
}
console.log(fruits); // 출력 결과: ["banana", "orange", "grape"]
```

위의 예제 코드처럼 `includes()` 메소드를 사용하여 특정 문자열이 배열에 포함되어 있는지 확인하고, `map()` 메소드로 해당 문자열을 대체할 수 있습니다. 

## 딥 다이브

텍스트 검색 및 대체는 실제로 내부적으로 어떻게 동작하는지 더 알아볼 필요가 있습니다. TypeScript에서 `replace()` 메소드는 내부적으로 정규표현식을 사용하여 문자열을 검색하고 대체하는 것으로 구현되어 있습니다. 따라서 정규표현식을 잘 이해하고 활용하는 것이 중요합니다. 또한 `replace()` 메소드의 첫 번째 인자로 사용된 문자열 대신에 정규표현식을 사용할 수도 있습니다.

## 또 보기

- [MDN web docs - Replace text using regular expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions#Replacing_text_using_regular_expressions)
- [TypeScript Official Documentation - String.prototype.replace()](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-4.html#the-stringprototypereplace-all-overload)