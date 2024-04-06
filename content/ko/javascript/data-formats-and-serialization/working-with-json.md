---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:33.434011-07:00
description: "\uC0AC\uC6A9 \uBC29\uBC95: JSON \uBB38\uC790\uC5F4\uC744 JavaScript\
  \ \uAC1D\uCCB4\uB85C \uBCC0\uD658\uD558\uB824\uBA74 `JSON.parse()`\uB97C \uC0AC\uC6A9\
  \uD558\uC138\uC694."
lastmod: '2024-03-13T22:44:55.822376-06:00'
model: gpt-4-0125-preview
summary: "JSON \uBB38\uC790\uC5F4\uC744 JavaScript \uAC1D\uCCB4\uB85C \uBCC0\uD658\
  \uD558\uB824\uBA74 `JSON.parse()`\uB97C \uC0AC\uC6A9\uD558\uC138\uC694."
title: "JSON\uACFC \uD568\uAED8 \uC77C\uD558\uAE30"
weight: 38
---

## 사용 방법:


### JSON 파싱하기
JSON 문자열을 JavaScript 객체로 변환하려면 `JSON.parse()`를 사용하세요.

```javascript
const jsonString = '{"name":"John", "age":30, "city":"New York"}';
const obj = JSON.parse(jsonString);
console.log(obj.name); // 출력: John
```

### JavaScript 객체 문자열화하기
JavaScript 객체를 다시 JSON 문자열로 변환하려면 `JSON.stringify()`를 사용하세요.

```javascript
const user = { name: "Jane", age: 25, city: "London" };
const jsonString = JSON.stringify(user);
console.log(jsonString); // 출력: {"name":"Jane","age":25,"city":"London"}
```

### Node.js에서 파일 다루기
Node.js 환경에서 JSON 파일을 읽고 객체로 변환하려면 `fs` 모듈을 사용할 수 있습니다. 이 예제는 `data.json`이라는 파일이 있다고 가정합니다.

```javascript
const fs = require('fs');

fs.readFile('data.json', 'utf-8', (err, data) => {
    if (err) throw err;
    const obj = JSON.parse(data);
    console.log(obj);
});
```

객체를 JSON 파일에 쓰기:

```javascript
const fs = require('fs');
const user = { name: "Mike", age: 22, city: "Berlin" };

fs.writeFile('user.json', JSON.stringify(user, null, 2), (err) => {
    if (err) throw err;
    console.log('파일에 데이터 쓰기 완료');
});
```

### 서드파티 라이브러리
복잡한 JSON 작업의 경우, `lodash`와 같은 프레임워크와 라이브러리는 작업을 단순화할 수 있지만, 기본적인 작업의 경우 네이티브 JavaScript 함수가 종종 충분합니다. 대규모 또는 성능이 중요한 애플리케이션의 경우, 더 빠른 JSON 문자열화를 위해 `fast-json-stringify` 또는 더 유연한 JSON 형식을 사용하여 파싱 및 문자열화를 위한 `json5`와 같은 라이브러리를 고려할 수 있습니다.

`json5`로 파싱하기:
```javascript
const JSON5 = require('json5');

const jsonString = '{name:"John", age:30, city:"New York"}';
const obj = JSON5.parse(jsonString);
console.log(obj.name); // 출력: John
```

이 예시들은 JavaScript에서 JSON을 다루는 기본적인 작업을 다루며, 다른 언어에서 전환한 초심자들이 웹 애플리케이션에서 데이터를 효율적으로 다룰 수 있도록 완벽합니다.
