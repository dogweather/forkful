---
title:                "문자열 연결하기"
html_title:           "Javascript: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

# 뭐고 왜 하는 거야?  
문자열 연결(concatenation)이 무엇인지와 프로그래머들이 왜 이를 하는지에 대해 설명할게. 문자열 연결은 두 개 이상의 문자열을 하나로 합치는 것을 말해. 예를 들어, "안녕하세요"와 "반가워요"라는 두 문자열을 연결하면 "안녕하세요반가워요"라는 하나의 문자열이 만들어지는 것이지. 프로그래머들은 문자열 연결을 사용해 긴 문장이나 내용을 만들거나 다양한 문자열 조합을 할 수 있어서 유용하게 활용해.

# 어떻게 하지?  
자바스크립트에서 문자열 연결은 "+" 기호를 사용해 할 수 있어. 이를 코드로 보면 다음과 같아:  
```javascript
let str1 = "안녕하세요";
let str2 = "반가워요";
let hello = str1 + str2;
console.log(hello); // "안녕하세요반가워요"
```
이외에도 다양한 방식으로 문자열 연결을 할 수 있어. 예를 들어, "문자열".concat("연결")을 사용하거나 `${변수}` 를 활용할 수도 있어.

# 더 살펴보기  
문자열 연결은 프로그래밍 언어에서 오래된 개념으로 자바스크립트뿐만 아니라 다른 언어에서도 많이 사용되고 있어. 또한, 배열을 사용해 문자열을 연결하는 방법도 많이 존재해. 이뿐만 아니라 문자열을 처리하는 다양한 기능들을 제공하는 라이브러리들도 있어서 필요에 따라 선택적으로 사용할 수 있어.

# 관련 자료  
- [자바스크립트 문자열 연결 메소드 - MDN](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/concat)  
- [자바스크립트 문자열 - MDN](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String)