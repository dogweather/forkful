---
title:                "Javascript: 제목: 텍스트 검색 및 교체"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜

문자열에서 특정 문자를 찾아 다른 문자로 대체하는 것은 프로그래밍에서 일반적으로 수행하는 작업 중 하나입니다. 이를 효율적으로 수행하려면 일부 지식이 필요합니다.

## 어떻게

문자열에서 텍스트를 찾아 대체하기 위해서는 먼저 `replace()` 함수를 사용해야 합니다. 이 함수는 두 개의 매개변수를 받습니다. 첫 번째 매개변수에는 대체할 문자열을, 두 번째 매개변수에는 새로 대체될 문자열을 전달합니다.

```javascript
"Hello, world!".replace("world", "Javascript"); // "Hello, Javascript!"
```

위의 예시에서는 `replace()` 함수를 사용하여 "Hello, world!"라는 문자열에서 "world"라는 문자열을 "Javascript"로 대체했습니다.

하지만 이렇게 단순한 대체 작업만으로는 충분하지 않습니다. 때로는 대소문자를 구분해야 하거나, 정규식을 사용해야 할 수도 있습니다. 이 경우에는 `replace()` 함수 대신 정규식의 `match()` 함수를 사용해야 합니다.

```javascript
"JavaScript is awesome!".replace(/javascript/i, "JavaScript"); // "JavaScript is awesome!"
```

위의 예시에서는 대문자인 "JavaScript"를 찾기 위해 정규식 플래그인 `i`를 사용하였습니다.

## 깊이 있는 정보

위의 예시들은 간단한 대체 작업이었지만, 프로그래밍에서는 더 복잡한 작업이 필요한 경우가 많습니다. 가령, 문자열에서 모든 일치하는 텍스트를 대체할 때도 있을 것입니다. 이때는 `replace()` 함수를 조금 변형하여 사용하면 됩니다.

```javascript
"1, 2, 3, 4".replace(/\d/g, "0"); // "0, 0, 0, 0"
```

위의 예시는 정규식의 `g` 플래그를 사용하여 모든 일치하는 숫자를 "0"으로 대체한 결과를 보여줍니다. 이 외에도 정규식을 이용하면 더 다양한 대체 작업을 수행할 수 있습니다.

## 더 알아보기

- [MDN - String.prototype.replace()](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [MDN - Regular Expressions](https://developer.mozilla.org/ko/docs/Web/JavaScript/Guide/Regular_Expressions)
- [W3Schools - JavaScript Strings](https://www.w3schools.com/js/js_strings.asp)
- [JavaScript.info - String Methods](https://javascript.info/string-methods)