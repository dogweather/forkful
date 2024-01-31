---
title:                "정규 표현식 활용하기"
date:                  2024-01-19
html_title:           "Arduino: 정규 표현식 활용하기"
simple_title:         "정규 표현식 활용하기"

category:             "Javascript"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
정규 표현식은 문자열에서 패턴을 찾기 위한 강력한 도구입니다. 프로그래머들은 데이터 검증, 검색, 파싱과 같은 작업을 자동화하고 효율화하기 위해 사용합니다.

## How to:
```javascript
const exampleString = 'Hello World! 123';

// 숫자 찾기
const regexNumbers = /\d+/g;
console.log(exampleString.match(regexNumbers)); // ['123']

// 'Hello' 단어 찾기
const regexHello = /Hello/;
console.log(regexHello.test(exampleString)); // true

// 대소문자 구분 없이 'world' 단어 찾기
const regexWorldCaseInsensitive = /world/i;
console.log(regexWorldCaseInsensitive.test(exampleString)); // true
```

## Deep Dive
정규 표현식은 1960년대에 켄 톰슨이 만들었습니다. 문자열 처리를 위한 대안으로 문자열 함수나 파싱 라이브러리가 있지만, 정규 표현식은 복잡한 검색을 단순하게 만듭니다. 자바스크립트에서는 RegExp 객체로 구현되어 있으며, 문자열의 `.match()`, `.replace()`, `.search()`, `.split()` 등의 메서드와 함께 사용됩니다.

## See Also
- [MDN 정규 표현식 가이드](https://developer.mozilla.org/ko/docs/Web/JavaScript/Guide/Regular_Expressions)
- [RegExr: 정규 표현식을 배우고 테스트하는 온라인 도구](https://regexr.com/)
- [정규 표현식 101: 온라인 정규 표현식 테스터 및 디버거](https://regex101.com/)
