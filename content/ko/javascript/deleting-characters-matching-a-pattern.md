---
title:                "Javascript: 패턴과 일치하는 문자 삭제"
simple_title:         "패턴과 일치하는 문자 삭제"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# 왜

자바스크립트 프로그래밍을 하다보면, 때때로 문자열에서 특정 패턴에 맞는 문자를 제거해야 할 때가 있습니다. 이럴 때는 어떤 이유로 이 작업을 해야 하는지 알아보겠습니다.

# 어떻게

문자열에서 특정 패턴을 찾아 제거하는 방법은 여러 가지가 있지만, 가장 쉽고 간단한 방법은 정규식(Regular Expression)을 사용하는 것입니다. 아래의 코드 예시를 참고해보세요.

```Javascript
// "Hello, world!" 문자열에서 모음을 제거하는 예시
let str = "Hello, world!";
let pattern = /[aeiou]/gi; // [aeiou]는 모음을 나타내는 정규식입니다. g는 모든 발생을, i는 대소문자를 구분하지 않도록 설정합니다.
let result = str.replace(pattern, ""); // 문자열에서 해당 정규식에 부합하는 모든 부분을 빈 문자열로 바꿉니다.
console.log(result); //Hll, wrld!
```

위 코드에서 볼 수 있듯이, 문자열에서 해당 패턴에 부합하는 문자를 제거하는 것은 정규식을 이용하여 간단하게 구현할 수 있습니다.

# 딥 다이브

다른 언어들과 마찬가지로, 자바스크립트에서도 정규식에 대한 많은 기능들을 제공하고 있습니다. 예를 들어, 패턴에 부합하는 문자를 선택적으로 제거할 수 있는 방법이나, 대체 문자열로 치환하는 방법 등이 있습니다. 이에 대한 자세한 내용은 공식 문서를 참고하는 것을 추천합니다.

# 함께 보기

- [정규식 튜토리얼 - MDN 문서](https://developer.mozilla.org/ko/docs/Web/JavaScript/Guide/Regular_Expressions)
- [정규식 연습 및 디버깅 사이트 - Regex101](https://regex101.com/)