---
title:                "json과 함께 작업하기"
html_title:           "TypeScript: json과 함께 작업하기"
simple_title:         "json과 함께 작업하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## 이게 뭔가요?

JSON은 Javascript Object Notation의 약자로, 데이터를 저장하고 교환하기 위한 형식 중 하나입니다. JSON의 간단한 구조와 유연함 때문에, 프로그래머들은 주로 쉽게 데이터를 다루기 위해 JSON을 사용합니다.

## 어떻게 하나요?

### JSON 데이터 생성

TypeScript에서 new 키워드를 사용하여 JSON 객체를 생성할 수 있습니다. 아래의 예시를 참고해보세요.

```TypeScript
let data = {
  name: "John",
  age: 25,
  city: "Seoul"
};
```

### 데이터 접근하기

저장된 데이터에 접근하기 위해서는 `.` 또는 `[]`를 사용할 수 있습니다. 예를 들어, `data.age`는 25를 반환합니다.

```TypeScript
console.log(data.name); // "John"
console.log(data["city"]); // "Seoul"
```

### JSON 문자열로 변환하기

JSON 객체는 `JSON.stringify()` 메소드를 사용하여 문자열로 변환할 수 있습니다. 아래 예시를 확인해보세요.

```TypeScript
let dataString = JSON.stringify(data);
console.log(dataString); // {"name":"John","age":25,"city":"Seoul"}
```

### 문자열을 JSON 객체로 변환하기

이번에는 `JSON.parse()` 메소드를 사용하여 문자열을 다시 JSON 객체로 변환하겠습니다. 아래 예시를 참고해보세요.

```TypeScript
let parsedData = JSON.parse(dataString);
console.log(parsedData.name); // "John"
```

## 깊이 파헤쳐보기

### 역사적 배경

JSON은 1999년에 더글라스 크락포드(Douglas Crockford)가 만든 형식으로, 원래는 자바스크립트에서 사용하기 위해 개발되었습니다. 하지만 지금은 다양한 프로그래밍 언어에서 사용되고 있습니다.

### 대안

JSON은 프로그래머들이 데이터를 저장하고 교환하기 위해 주로 사용되는 형식 중 하나지만, XML과 같은 다른 형식도 존재합니다. 이러한 형식들은 각자 장단점이 있기 때문에 문제의 종류와 상황에 맞게 선택하여 사용해야 합니다.

## 참고 자료

- [JSON 공식 사이트](https://www.json.org/)
- [JSON과 XML 비교](https://stackoverflow.com/questions/4862310/why-json-over-xml)