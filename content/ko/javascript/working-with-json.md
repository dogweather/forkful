---
title:                "Javascript: json 작업하기"
simple_title:         "json 작업하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/working-with-json.md"
---

{{< edit_this_page >}}

# 왜 JSON을 사용해야 하는가?

JSON(JavaScript Object Notation)은 오늘날 웹 개발에서 필수적인 데이터 형식이 되었습니다. JSON은 사람과 기계 모두에게 읽고 쓰기 쉬우며 텍스트로 구성되기 때문에 데이터를 전송하고 저장하는 데에 적합합니다.

## 사용 방법

JavaScript에서 JSON을 다루는 것은 매우 간단합니다. 예를 들어, 다음과 같은 JSON 데이터가 있다고 가정해봅시다.

```Javascript
// JSON 데이터
var person = {
    "name": "John",
    "age": 30,
    "city": "Seoul"
};
```

이 데이터를 JavaScript에서 사용하기 위해서는 다음과 같은 코드를 작성하면 됩니다.

```Javascript
// JSON 데이터 접근
console.log(person.name); // 출력 결과: John
console.log(person.age); // 출력 결과: 30
console.log(person.city); // 출력 결과: Seoul
```

## 깊게 들어가보기

JSON은 JavaScript에서 객체를 생성하고 데이터를 저장하는 데에 매우 유용한 형식입니다. JSON 데이터는 일련의 key-value 쌍으로 구성되어 있는데, 이를 객체로 표현하여 사용할 수 있습니다. 또한, JSON 데이터는 배열로 구성될 수도 있습니다.

JSON 데이터의 key는 큰따옴표 "" 안에 쓰여야 하며, value는 숫자, 문자열, 불리언, 배열, 객체 등 다양한 형식이 올 수 있습니다. 또한, JSON 데이터를 다룰 때 유의해야 할 점은 값의 끝에 쉼표 "," 를 붙이지 않는 것입니다.

# 더 알아보기

JSON은 웹 개발에서 필수적인 데이터 형식이므로, 정확히 이해하고 다룰 수 있도록 더 많은 학습이 필요합니다. 아래는 JSON 데이터를 다루는 데에 도움이 될 만한 자료들입니다.

[JSON 공식 서식 가이드라인](https://www.json.org/json-ko.html)
[JSON 데이터 다루기 예제](https://www.tutorialspoint.com/json/)
[JSON과 JavaScript 간의 변환 방법](http://www.bejson.com/json/json2js/)
[JSON을 사용하는 실제 프로젝트 예시](https://github.com/d3/d3/wiki/Gallery)
[웹 개발에서 JSON의 활용 방법](https://www.webdesignerdepot.com/2008/11/15-simple-steps-to-understand-json/)
[더 많은 자료를 알아보려면...](https://www.google.com/search?q=JSON+tutorial)