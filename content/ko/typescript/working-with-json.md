---
title:                "TypeScript: json 사용하기"
simple_title:         "json 사용하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## 왜

JSON(JavaScript Object Notation)은 현대의 웹 애플리케이션에서 데이터를 다루는 데에 광범위하게 사용되는 데이터 포맷입니다. 이러한 이유로 TypeScript 개발자 여러분은 중요한 데이터를 다룰 때 JSON을 사용하는 것이 좋습니다.

## 어떻게

데이터를 JSON 형식으로 다루는 것은 TypeScript에서 간단하게 할 수 있습니다. 우선, `JSON.parse()` 함수를 사용하여 JSON 문자열을 JavaScript 객체로 분석할 수 있습니다. 또한, `JSON.stringify()` 함수를 사용하여 JavaScript 객체를 JSON 문자열로 변환할 수 있습니다.

```TypeScript
// JSON 문자열 분석
const jsonString = '{"name": "John", "age": 30}';
const person = JSON.parse(jsonString);
console.log(person.name); // "John"

// JavaScript 객체를 JSON 문자열로 변환하기
const car = { make: "Toyota", model: "Corolla", year: "2020" };
const carString = JSON.stringify(car);
console.log(carString); // '{"make": "Toyota", "model": "Corolla", "year": "2020"}'
```

## 깊이 파고들기

JSON은 매우 자유로운 형식이기 때문에 다양한 종류의 데이터를 다룰 수 있습니다. 하지만 이러한 유연성 때문에 JSON 데이터를 다루는 것은 종종 복잡하고 어려운 과정일 수 있습니다. 따라서, 항상 데이터 스트럭쳐에 대한 깊은 이해가 필요합니다. 또한, TypeScript에서는 인터페이스와 제네릭을 사용하여 JSON 데이터를 구조화할 수 있습니다.

## 더 알아보기

- [JSON 공식 문서](https://www.json.org/json-en.html)
- [TypeScript에서 JSON 다루기](https://blog.logrocket.com/json-in-typescript/)
- [인터페이스와 제네릭을 이용한 JSON 데이터 구조화](https://medium.com/weboptimer/write-better-typescript-json-parsers-with-type-mapping-decoding-made-easy-d427c33b6fa)<br/><br/>

## 참고

- [TypeScript](https://www.typescriptlang.org/)
- [JSON](https://www.json.org/json-en.html)