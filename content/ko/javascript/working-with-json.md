---
title:                "JSON 사용하기"
html_title:           "Javascript: JSON 사용하기"
simple_title:         "JSON 사용하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/working-with-json.md"
---

{{< edit_this_page >}}

## 뭘? 왜?

JSON은 자바스크립트와 다른 프로그래밍 언어 간에 데이터를 교환하기 위한 가벼운 형식입니다. 프로그래머들은 이를 사용하여 데이터를 구조화하고 전송하기 쉽게 만들기 위해 사용합니다.

## 어떻게:

```Javascript
// JSON 형식의 데이터 생성
var person = {
  name: "John",
  age: 30,
  occupation: "Developer"
};

// JSON 형식의 데이터 출력
console.log(JSON.stringify(person)); 

// JSON 데이터를 자바스크립트 객체로 변환
var data = '{"id": 1, "name": "Jane", "occupation": "Designer"}';
var person2 = JSON.parse(data);
console.log(person2.name); // "Jane"
```

## 깊게 파헤치기:

JSON은 2001년에 더글라스 크락포드(Douglas Crockford)가 만든 형식이며, 기존의 XML보다 가볍고 간결한 형식으로 많이 사용되고 있습니다. 대안으로는 XML, YAML, CSV 등이 있지만, JSON의 단순성과 높은 호환성으로 인해 다른 형식들에 비해 많은 사용자들이 있습니다. 자바스크립트에서는 위 예시와 같이 JSON을 다루기 위한 내장 함수인 `JSON.stringify()`와 `JSON.parse()`가 있습니다.

## 더 알아보기:

- [JSON 공식 홈페이지](https://www.json.org/json-ko.html)
- [JSON의 장단점 비교하기](https://www.mobilize.net/blog/bid/66525/Comparing-XML-and-JSON)