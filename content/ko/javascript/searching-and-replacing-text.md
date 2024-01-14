---
title:                "Javascript: 텍스트 검색 및 교체"
simple_title:         "텍스트 검색 및 교체"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

왜: 현재 사용되는 텍스트를 검색하고 바꾸는 것에 대해 배우는 것이 왜 중요한지 궁금하시다면,이 포스트를 읽어보세요.

## 왜

텍스트를 검색하고 바꾸는 것은 개발 과정에서 매우 중요합니다. 코드를 작성하는 동안 잘못된 변수명이나 오탈자 등을 수정해야 할 때가 있습니다. 또는 대규모 데이터를 처리하거나 웹 애플리케이션에서 사용자가 입력한 정보를 처리해야 할 때 특정 텍스트를 쉽게 찾고 바꿀 수 있는 기능이 필요합니다.

## 어떻게

텍스트를 검색하고 바꾸는 것은 Javascript에서 매우 간단합니다. 우선 `replace()` 메소드를 사용하여 검색할 텍스트와 바꿀 텍스트를 지정해줍니다. 예를 들어, `replace("안녕", "Hello")`는 문자열에서 "안녕"이라는 텍스트를 찾아 "Hello"로 바꿔줍니다.

```Javascript
let str = "안녕하세요!";

console.log(str.replace("안녕", "Hello")); 

// Output: Hello하세요!
```

텍스트를 변경할 때 대소문자를 구분하고 싶지 않다면 `replace()` 메서드에 정규식을 넣을 수도 있습니다. 예를 들어, `/안녕/i`는 대소문자를 구분하지 않고 "안녕"을 찾아 바꿔줍니다.

```Javascript
let str = "안녕하세요!";

console.log(str.replace(/안녕/i, "Hello")); 

// Output: Hello하세요!
```

찾은 모든 텍스트를 한 번에 바꾸고 싶다면 정규식에 `/g`를 추가하여 전역 검색을 할 수 있습니다. 아래 예제는 "안녕"이라는 모든 텍스트를 "Hello"로 바꿔줍니다.

```Javascript
let str = "안녕하세요! 안녕하세요!";

console.log(str.replace(/안녕/g, "Hello")); 

// Output: Hello하세요! Hello하세요!
```

## 딥 다이브

더 많은 정보를 알고 싶다면 MDN 문서에서 `replace()` 메소드에 대해 더 자세히 배울 수 있습니다. `split()` 메소드와 함께 사용하면 좀 더 복잡한 텍스트 검색과 교체를 할 수 있다는 것을 깨달을 수 있습니다. 혹은 정규식을 사용하여 더욱 더 강력한 텍스트 검색과 교체를 할 수 있습니다.

## 관련 링크

- [MDN – replace()](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [MDN – split()](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/split)
- [정규식 튜토리얼](https://regexone.com/)