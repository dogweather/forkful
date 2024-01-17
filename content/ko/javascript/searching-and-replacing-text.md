---
title:                "텍스트 검색과 대체"
html_title:           "Javascript: 텍스트 검색과 대체"
simple_title:         "텍스트 검색과 대체"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

##서론
자바스크립트 프로그래밍은 현대적인 웹 개발에서 필수적인 요소입니다. 이 언어를 사용하여 웹 페이지를 만들고 사용자와 상호작용하게 할 수 있습니다. 이 문서에서는 일반적인 자바스크립트 개발에서 사용되는 하나의 기술인 "텍스트 검색 및 변경"에 대해 알아보겠습니다. 이를 통해 당신도 좀 더 전문적인 프로그래머가 될 수 있을 것입니다.

##무엇인가? 왜 하나요?
텍스트 검색 및 변경은 문자열을 분석하고 원하는 대상을 찾아 내용을 변경하는 기술입니다. 프로그래머들은 이를 사용하여 특정 문자열을 찾아 원하는 형태로 변경하거나 조건에 따라 단어를 대체하는 등 다양하게 활용할 수 있습니다. 이를 통해 코드의 일관성을 유지하고 작업을 효율적으로 할 수 있습니다.

##방법:
일반적으로 자바스크립트에서는 문자열의 특정 부분을 찾고 변경하는 데에는 "replace" 메소드를 사용합니다. 다음은 간단한 예제 코드입니다. 

```Javascript
let inputString = "Hello, world!";
let newString = inputString.replace("world", "Korea");

console.log(newString); // output: Hello, Korea!
```

위의 코드에서는 "world"라는 문자열을 "Korea"로 대체하였으며, "Hello, world!"라는 원래 문자열 대신 "Hello, Korea!"라는 변환된 문자열을 출력하게 됩니다.

##더 깊이 들어가보기:
자바스크립트에서의 텍스트 검색 및 변경 기술은 매우 다양한 방법으로 활용될 수 있습니다. 예를 들어, 정규표현식을 이용하여 문자열을 패턴에 따라 검색하고 변경할 수도 있습니다. 또한, 기존 문자열을 변경하는 대신 새로운 문자열을 생성하는 "substring" 메소드 등 다양한 메소드를 사용할 수도 있습니다. 이 외에도 다른 프로그래밍 언어에서도 비슷한 기능을 수행하는 라이브러리 또는 함수들이 존재합니다.

##참고자료:
- [MDN web docs - replace](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [MDN web docs - substring](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [Regular-Expressions.info](https://www.regular-expressions.info/)