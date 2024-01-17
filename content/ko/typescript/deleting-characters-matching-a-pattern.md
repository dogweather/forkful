---
title:                "패턴과 일치하는 문자 삭제하기."
html_title:           "TypeScript: 패턴과 일치하는 문자 삭제하기."
simple_title:         "패턴과 일치하는 문자 삭제하기."
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

"## 무엇 & 왜?"
특정 패턴과 일치하는 문자를 삭제하는 것은, 프로그래머들이 특정 문자들에 대한 조작 또는 필터링을 위해 사용하는 기술입니다. 이를 통해 코드의 가독성을 개선하고, 불필요한 문자를 제거함으로써 프로그램의 성능을 향상시킬 수 있습니다.

"## 어떻게:"
```TypeScript
let text = "Hello, World!";
text = text.replace(/[aeiou]/gi, "");
console.log(text); // Hll, Wrld!
```
해당 코드는 문자열 "Hello, World!"에서 모음을 삭제하는 예제입니다. 우선, `replace` 함수를 이용하여 해당 문자열에서 `[aeiou]` 패턴과 일치하는 모든 문자들을 빈 문자로 변경합니다. 이후 `console.log` 함수를 통해 변경된 문자열이 출력됩니다.

"## 깊이 파헤치기:"
(1) 문자 삭제 기술은 과거에도 많이 사용되었습니다. 하지만 이제는 더 단순하고 효율적인 방법들이 개발되었습니다. (2) 문자 삭제를 위해 정규표현식이 아닌 `split` 함수와 `join` 함수를 사용하는 방법도 있습니다. (3) 이 기술의 구현 방법은 다양하지만, 대부분의 경우 정규표현식을 활용하여 문자를 삭제하는 것이 가장 간단하고 효율적입니다.

"## 또 다른 참고자료:"
- [정규표현식에 대한 이해](https://developer.mozilla.org/ko/docs/Web/JavaScript/Guide/Regular_Expressions)
- [브라우저에서의 문자 삭제 기술](https://www.w3schools.com/jsref/jsref_replace.asp)
- [ES6 이후 문자 조작 기술 소개](https://www.digitalocean.com/community/tutorials/js-more-string-operations),