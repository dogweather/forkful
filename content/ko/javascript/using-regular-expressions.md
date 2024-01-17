---
title:                "정규 표현식 사용하기"
html_title:           "Javascript: 정규 표현식 사용하기"
simple_title:         "정규 표현식 사용하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

정규 표현식을 사용하는 것은 프로그래머들에게 강력한 도구입니다. 정규 표현식은 문자열 안에서 특정 패턴을 찾고, 추출하거나 대체할 수 있는 기능을 제공합니다. 이를 통해 더욱 간편하게 문자열 처리를 할 수 있습니다.

## 어떻게:

```Javascript
// 문자열 안에서 특정 패턴을 찾고, 추출하는 예제
const myString = "안녕하세요! 제 이름은 John입니다.";
const regex = /이름은 (\w+)/; // 정규 표현식을 사용해서 이름을 추출
const result = regex.exec(myString);
console.log(result[1]); // "John"이 출력됨
```

```Javascript
// 문자열 안에서 특정 패턴을 찾고, 대체하는 예제
const myString = "안녕하세요! 오늘의 날씨는 매우 좋아요.";
const regex = /좋아요/;
const result = myString.replace(regex, "나빠요"); // 패턴과 일치하는 부분을 "나빠요"로 대체
console.log(result); // "안녕하세요! 오늘의 날씨는 매우 나빠요."가 출력됨
```

## 깊게 파헤치기:

1. 정규 표현식은 1956년 미국 과학자인 Stephen Kleene이 고안했습니다. 이후 많은 프로그래밍 언어들에서 지원하게 되었고, 현재는 여러 다양한 언어에서 사용할 수 있습니다.

2. 정규 표현식 외에도 문자열 처리를 위해 다른 방법들도 있지만, 정규 표현식은 보다 간편하고 효율적인 방법을 제공합니다. 다만, 조금 더 복잡한 패턴을 처리할 때는 어렵고 혼란스러울 수 있습니다.

3. 정규 표현식을 효율적으로 사용하기 위해서는 패턴에 대한 이해가 필요합니다. 이를 위해 정규 표현식의 구조와 사용 방법에 대해 공부하고, 다양한 패턴을 연습해보는 것이 좋습니다.

## 더 보기:

- [MDN 정규 표현식 문서](https://developer.mozilla.org/ko/docs/Web/JavaScript/Guide/Regular_Expressions): 정규 표현식에 대한 기본적인 개념과 사용 방법을 학습할 수 있는 문서입니다.
- [정규 표현식을 이용한 검색과 치환](https://www.regular-expressions.info/): 보다 복잡한 패턴을 다루는 방법을 학습할 수 있는 유용한 튜토리얼입니다.