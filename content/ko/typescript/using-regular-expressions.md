---
title:                "정규식을 사용하는 방법"
html_title:           "TypeScript: 정규식을 사용하는 방법"
simple_title:         "정규식을 사용하는 방법"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜

정규 표현식을 사용하는 이유의 목적은 입력된 문자열에서 원하는 패턴을 찾기 위해서 입니다. 이를 통해 빠르고 정확하게 원하는 데이터를 추출할 수 있습니다.

## 사용 방법

```TypeScript
// 단어 'apple'을 검색하는 정규 표현식
let re = /apple/;

// 문자열에서 정규 표현식과 일치하는 단어 찾기
let result = re.exec("I love apples");
console.log(result[0]); // 'apple'
```

위의 예시에서 볼 수 있듯이, 정규 표현식은 `/`사이에 원하는 패턴을 작성하여 사용합니다. `exec` 메소드는 일치하는 첫 번째 결과를 배열로 반환하고, `result[0]`을 통해 해당 결과에 접근할 수 있습니다.

```TypeScript
// 숫자만을 가진 문자열에서 숫자를 찾는 정규 표현식
let re = /\d+/;

// 문자열에서 숫자만을 추출하여 배열로 반환
let result = "I have 4 apples".match(re);
console.log(result); // ['4']
```

또 다른 예시로, `match` 메소드를 사용하여 정규 표현식과 일치하는 부분을 추출하는 방법을 보여드렸습니다. `/` 사이에 있는 `\d+`는 0부터 9까지의 숫자 중 하나 이상이 있는지를 검사하는 패턴입니다. 따라서 `"I have 4 apples"`에서 `4`만 추출됩니다.

## 깊이 알아보기

정규 표현식은 문자열에서 패턴을 찾는 데에만 사용되는 것이 아닙니다. 많은 프로그래밍 언어에서 정규 표현식을 지원하고 있으며, 여러분의 필요에 따라 파일 검색, 데이터 분석 등 다양한 용도로 사용할 수 있습니다.

또한 정규 표현식에서는 다양한 메타 문자와 플래그를 사용할 수 있습니다. 메타 문자는 특별한 의미를 가지고 있으며, 주로 검색 패턴을 더 유연하게 만들어줍니다. 플래그는 검색 대상에 대한 옵션을 설정하는 역할을 하며, 예를 들어 대소문자 구분 여부를 결정할 수 있습니다.

## 참고

- [MDN 정규 표현식](https://developer.mozilla.org/ko/docs/Web/JavaScript/Guide/Regular_Expressions)
- [정규 표현식 테스트 사이트](https://regexr.com/)