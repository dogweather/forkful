---
title:                "정규 표현식 활용하기"
date:                  2024-01-19
html_title:           "Arduino: 정규 표현식 활용하기"
simple_title:         "정규 표현식 활용하기"

category:             "TypeScript"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
정규 표현식은 문자열에서 패턴을 찾기 위한 강력한 도구입니다. 프로그래머는 데이터 검증, 검색 및 문자열 조작을 효율적으로 하기 위해 이를 사용합니다.

## How to: (방법)
```TypeScript
// 문자열에서 숫자 찾기
const str = '우리는 2023년 4월에 코딩을 합니다.';
const regex = /\d+/g;

const result = str.match(regex);
console.log(result); // ['2023', '4']

// 문자열 치환하기
const replacedStr = str.replace(regex, '[숫자]');
console.log(replacedStr); // '우리는 [숫자]년 [숫자]월에 코딩을 합니다.'
```

## Deep Dive (심층 분석)
과거에는 문자열 처리가 비교적 단순했지만, 데이터의 복잡성이 증가함에 따라 정규 표현식의 중요성이 더욱 높아졌습니다. 대안으로 문자열 함수(예: `indexOf`나 `slice`)를 사용할 수 있지만 이들은 복잡한 검색 패턴에 비효율적입니다. TypeScript에서는 자바스크립트의 정규 표현식을 그대로 사용할 수 있으며, 패턴 매칭과 관련한 모든 기능은 내부적으로 `RegExp` 객체를 통해 이루어집니다.

## See Also (더보기)
- [MDN 정규 표현식 가이드](https://developer.mozilla.org/ko/docs/Web/JavaScript/Guide/Regular_Expressions)
- [정규 표현식 테스팅 도구 Regex101](https://regex101.com/)
- [TypeScript Handbook: Regular Expressions](https://www.typescriptlang.org/docs/handbook/2/objects.html#regex-matching)
