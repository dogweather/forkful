---
title:                "부분 문자열 추출하기"
html_title:           "TypeScript: 부분 문자열 추출하기"
simple_title:         "부분 문자열 추출하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

# 무엇 & 왜?

추출된 서브스트링이란 무엇인지, 그리고 왜 프로그래머들이 이를 하는지에 대해 2~3 문장으로 설명합니다.

## 추출된 서브스트링이란?

추출된 서브스트링은 문자열에서 원하는 부분의 일부를 가져오는 것을 의미합니다. 예를 들어, 문자열 "Hello World"에서 "Hello"만 따로 추출할 수 있습니다.

## 왜 프로그래머들은 이를 하는가?

서브스트링을 추출하는 것은 문자열을 조작하고 처리하는 데 유용한 도구입니다. 이를 통해 원하는 부분만 따로 처리하거나, 특정 패턴을 가진 문자열을 쉽게 찾을 수 있습니다.

## 사용 방법:

다음은 추출된 서브스트링을 TypeScript에서 사용하는 예시 코드와 출력 결과입니다.

### 기본 형식:

```TypeScript
let str = "Hello World";
let subStr = str.substring(0, 5);
console.log(subStr); // 출력 결과: "Hello"
```

`substring()` 메소드는 첫 번째 매개변수로 시작 위치를, 두 번째 매개변수로 끝 위치를 전달합니다. 이를 통해 문자열의 원하는 부분을 추출할 수 있습니다.

## 깊이 들어가기:

### 역사적 배경:

서브스트링 추출은 일반적인 프로그래밍 기법 중 하나입니다. 초기의 프로그래밍 언어에서부터 사용되어 왔고, 지금은 거의 모든 프로그래밍 언어에서 이 기능을 제공하고 있습니다.

### 대체 방법:

서브스트링을 추출하는 다른 방법으로는 `slice()` 메소드를 사용하는 것이 있습니다. `substring()`과 마찬가지로 정해진 위치에 따라 문자열을 추출할 수 있습니다.

### 구현 세부 사항:

`substring()` 메소드는 정해진 위치의 문자열을 반환하기 때문에, 문자열의 길이에 상관 없이 항상 동일한 결과를 반환합니다. 하지만 `slice()` 메소드는 음수 값을 매개변수로 전달할 수 있고, 이를 통해 문자열의 뒤에서부터 추출할 수 있습니다.

## 참고 자료:

- [MDN - substring()](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [MDN - slice()](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/slice)