---
title:                "json 작업하기"
html_title:           "Javascript: json 작업하기"
simple_title:         "json 작업하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/working-with-json.md"
---

{{< edit_this_page >}}

## 왜

JSON은 자바스크립트에서 데이터를 교환하는 데에 널리 사용되는 형식입니다. 이를 사용하면 데이터를 쉽게 읽고 사용할 수 있으며, 전송 및 저장 과정에서도 유용하게 사용될 수 있습니다.

## 방법

JSON은 일반적으로 객체 형식으로 작성되며, 키와 값 쌍으로 구성됩니다. 예를 들어 다음과 같이 작성할 수 있습니다.

```Javascript
let myObj = {
  name: "John",
  age: 25,
  profession: "Developer"
}

console.log(JSON.stringify(myObj));
```

위 코드에서는 `JSON.stringify()` 메소드를 사용하여 객체를 문자열 형태로 변환하고, `console.log()`를 사용하여 해당 결과를 출력합니다.

출력 결과는 다음과 같습니다.

```Javascript
{"name":"John","age":25,"profession":"Developer"}
```

JSON 형식은 보기 쉽고, 다루기 쉽습니다. 따라서 자바스크립트에서 데이터를 다룰 때 유용하게 활용될 수 있습니다.

## 깊이 들어가기

JSON은 텍스트 형식으로 데이터를 저장하므로, 다양한 프로그래밍 언어 간에도 쉽게 교환될 수 있습니다. 예를 들어 서버와 클라이언트 간에 데이터를 주고받을 때 많이 사용됩니다.

또한 JSON은 많은 유용한 메소드들을 제공합니다. 위에서 언급한 `JSON.stringify()` 외에도 `JSON.parse()`를 사용하여 문자열 형태의 JSON 데이터를 자바스크립트 객체로 변환할 수 있습니다.

## 연관된 링크

- [MDN JSON 문서](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/JSON)
- [JSON Tutorial](https://www.tutorialspoint.com/json/)
- [JSON Validator](https://jsonlint.com/)