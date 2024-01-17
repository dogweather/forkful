---
title:                "문자열 대문자화"
html_title:           "Javascript: 문자열 대문자화"
simple_title:         "문자열 대문자화"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# 왜 대문자화를 하는가?
대문자화란 문자열의 첫 번째 글자를 대문자로 변환하는 것을 말합니다. 프로그래머들은 이를 보통 변수 이름이나 함수 이름 등의 명명 규칙을 준수하기 위해 사용합니다.

# 대문자화하는 방법:
자바스크립트에서는 toUpperCase() 메소드를 사용하여 문자열의 첫 번째 글자를 대문자로 변환할 수 있습니다.

```Javascript
let name = "john";
console.log(name.toUpperCase());

// Output: JOHN
```

만약 문자열 중간에 있는 첫 번째 글자만 대문자로 변환하고 싶다면, 인덱싱(indexing)을 사용하여 해당 위치의 문자를 대문자로 변경할 수 있습니다.

```Javascript
let sentence = "hello world";
let upperCaseSentence = sentence.charAt(0).toUpperCase() + sentence.slice(1);
console.log(upperCaseSentence);

// Output: Hello world
```

# 깊게 들어가보면:
대문자화는 일반적으로 변수나 함수의 명명 규칙을 따르기 위해 사용되지만, 예외적인 상황에서는 매우 유용하게 사용될 수 있습니다. 예를 들어, 사용자의 이름을 나타내는 변수의 경우에는 첫 번째 글자를 대문자로 변환하여 좀 더 보기 쉽게 표시할 수 있습니다. 또한, 다른 언어나 플랫폼에서도 대문자화를 사용할 수 있는 경우가 있습니다.

프로그래밍 언어마다 대문자화를 수행하는 방법은 다를 수 있지만, 기본적으로 대문자화 메소드를 제공하기 때문에 쉽게 구현할 수 있습니다.

# 참고 자료:
- [JavaScript toUpperCase() 메소드 - MDN](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [How to Capitalize the First Letter of a String in JavaScript - W3schools](https://www.w3schools.com/jsref/jsref_touppercase.asp)