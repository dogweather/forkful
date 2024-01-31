---
title:                "JSON 다루기"
date:                  2024-01-19
html_title:           "Arduino: JSON 다루기"
simple_title:         "JSON 다루기"

category:             "TypeScript"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가요? 왜 사용하나요?)
JSON은 데이터를 저장하고 전송할 때 사용하는 경량의 데이터 형식입니다. 프로그래머들은 간결하고 읽기 쉬운 구조로 웹 API 통신과 설정 파일 생성에 JSON을 자주 사용합니다.

## How to (어떻게 사용하나요?)
```TypeScript
// JSON 객체 생성
const user = {
  name: "김철수",
  age: 30,
  isAdmin: true
};

// JSON으로 변환
const jsonString = JSON.stringify(user);
console.log(jsonString); // {"name":"김철수","age":30,"isAdmin":true}

// JSON 문자열 파싱
const userParsed = JSON.parse(jsonString);
console.log(userParsed); // { name: '김철수', age: 30, isAdmin: true }
```

## Deep Dive (깊이 파보기)
JSON, JavaScript Object Notation의 약자로, 2001년 Douglas Crockford에 의해 고안되었습니다. XML 같은 다른 데이터 형식과 비교했을 때, JSON은 더 가벼우며 자바스크립트와의 호환성이 뛰어납니다. 프로그래머는 `JSON.stringify`로 자바스크립트 객체를 JSON 문자열로 변환하고, `JSON.parse`로 JSON 문자열을 자바스크립트 객체로 파싱할 수 있습니다.

## See Also (참고자료)
- [MDN의 JSON 가이드](https://developer.mozilla.org/ko/docs/Learn/JavaScript/Objects/JSON)
- [JSON 공식 웹사이트](https://www.json.org/json-en.html)
- [TypeScript 핸드북](https://www.typescriptlang.org/docs/handbook/intro.html)
