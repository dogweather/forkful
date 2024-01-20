---
title:                "JSON 다루기"
html_title:           "Arduino: JSON 다루기"
simple_title:         "JSON 다루기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON은 JavaScript Object Notation의 약자로, 데이터를 저장하고 전달하기 위한 경량의 텍스트 기반 데이터 포맷입니다. 프로그래머들은 웹 API와의 통신, 설정 파일 작성, 데이터 직렬화 및 역직렬화를 위해 JSON을 사용합니다.

## How to:
```Javascript
// JSON 문자열을 JavaScript 객체로 변환하기.
let jsonString = '{"name":"홍길동","age":30}';
let jsonObject = JSON.parse(jsonString);
console.log(jsonObject.name); // 출력: 홍길동

// JavaScript 객체를 JSON 문자열로 변환하기.
let jsObject = {name: "이순신", age: 70};
let jsonOutput = JSON.stringify(jsObject);
console.log(jsonOutput); // 출력: {"name":"이순신","age":70}
```

## Deep Dive
JSON은 2001년에 Douglas Crockford에 의해 개발되었으며, XML을 대체하는 더 가볍고 간단한 데이터 교환 방식으로 널리 채택되었습니다. 대안으로는 YAML이나 XML이 있으나, JSON은 구문이 간결하고 파싱이 빠른 장점으로 인해 기본 선택지가 되었습니다. 구현 세부사항으로는 내장된 `JSON.parse()`와 `JSON.stringify()` 메소드를 주로 사용하여 JSON 데이터를 다룹니다.

## See Also
- MDN의 JSON 가이드: [MDN JSON Guide](https://developer.mozilla.org/ko/docs/Learn/JavaScript/Objects/JSON)
- JSON 공식 사이트: [JSON.org](http://json.org/)