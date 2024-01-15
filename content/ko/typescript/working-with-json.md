---
title:                "Json으로 작업하기"
html_title:           "TypeScript: Json으로 작업하기"
simple_title:         "Json으로 작업하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## 왜?

JSON은 개발자들이 데이터를 효율적으로 전달하고 이해하기 위해 자주 사용되는 데이터 형식입니다. 따라서 JSON을 다루는 능력은 현대 웹 개발에 필수적입니다.

## 어떻게?

JSON을 TypeScript에서 다루는 것은 간단합니다. 먼저, `import`문을 사용하여 `JSON` 모듈을 불러옵니다.

```
// TypeScript  import 문
import JSON from 'json';
```

그런 다음, `JSON.parse()` 함수를 사용하여 JSON 형식의 데이터를 파싱하고, `JSON.stringify()`를 사용하여 JavaScript 데이터를 JSON 형식의 문자열로 변환할 수 있습니다.

```
// TypeScript 코드 예제
const jsonStr = `{"name": "John", "age": 25, "hobby": "coding"}`;

// JSON 파싱
const data = JSON.parse(jsonStr);

// JSON 문자열로 변환
const newData = JSON.stringify(data);

console.log(newData); // 출력: {"name": "John", "age": 25, "hobby": "coding"}
```

## Deep Dive

TypeScript에서 JSON을 다루는 데에는 몇 가지 유용한 메소드가 있습니다.

### `JSON.parse()`

`JSON.parse()` 함수는 JSON 형식의 문자열을 파싱하여 JavaScript 데이터로 변환합니다. 만약 파싱에 실패하면 `SyntaxError`를 발생시킵니다.

### `JSON.stringify()`

`JSON.stringify()` 함수는 JavaScript 데이터를 JSON 형식의 문자열로 변환합니다. 필요에 따라 보기 좋은 형식으로 출력할 수 있도록 `space` 매개변수를 지정해줄 수 있습니다.

### `JSON.parse(text[, reviver])`

`JSON.parse()` 함수의 두 번째 매개변수인 `reviver`를 사용하면 JSON의 키 및 값에 대한 변환 처리를 할 수 있습니다. 이를 활용하면 파싱한 데이터에 대한 추가적인 로직을 구현할 수 있습니다.

## See Also
- [MDN - JSON](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/JSON)
- [TypeScript Playground](https://www.typescriptlang.org/play)
- [JSON Lint - JSON 형식 확인 사이트](https://jsonlint.com/)