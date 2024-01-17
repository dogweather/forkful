---
title:                "문자열을 소문자로 변환하기"
html_title:           "TypeScript: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

- 문자열을 소문자로 변환하는 것은 우리가 난해한 코드를 다루기 쉽게하기 위한 방법 중 하나입니다.
- 프로그래머들은 소문자로 변환함으로써 대소문자 구분 없이 데이터를 처리하고 비교할 수 있기 때문에 이 작업을 수행합니다.

## 하는 법:

```TypeScript
const str = "HeLlO WoRlD!";
const lowerCaseStr = str.toLowerCase();

console.log(lowerCaseStr);
// output: "hello world!"
```

## 깊게 들어가보기:

- 소문자로 변환하는 방법은 오래전부터 존재했으며, 문자열 처리와 관련된 기본적인 방법 중 하나입니다.
- 대소문자를 구분하지 않고 문자열을 비교하거나 검색하기 위해 알고리즘에서 이 작업을 자주 사용합니다.
- 다른 대안으로는 정규표현식을 사용하는 것이 있지만, 이 경우 정규표현식을 작성하는 데 시간이 더 걸릴 수 있습니다.

## 관련 자료:

- 소문자로 변환하는 방법에 대한 자세한 설명: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase
- 정규표현식을 사용하여 문자열 변환하기: https://regexr.com/