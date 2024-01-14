---
title:                "Javascript: 정규 표현식 사용하기"
simple_title:         "정규 표현식 사용하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

# 왜

정규 표현식을 사용하는 이유는 다음과 같습니다. 일반적으로 코드에서 문자열과 패턴을 비교하거나 찾는 작업이 필요할 때 유용합니다. 예를 들어, 이메일 주소나 전화번호와 같은 패턴을 찾거나, 특정 문자열이 포함된 단어를 필터링할 때 정규 표현식을 사용할 수 있습니다.

# 방법

아래는 정규 표현식을 사용하는 간단한 예제 코드와 그 결과입니다. 코드는 Javascript 문법을 따르며, 입력된 문자열 중에서 "hello"라는 단어가 포함되어 있는 경우에만 결과를 출력하는 예제입니다.

```Javascript
let str = "안녕하세요, hello, 잘가";
let pattern = /hello/;

if (pattern.test(str)) {
  console.log("문자열 안에 hello가 포함되어 있습니다.");
} else {
  console.log("문자열 안에 hello가 포함되어 있지 않습니다.");
}
```

결과:

```
문자열 안에 hello가 포함되어 있습니다.
```

위의 예제는 단순하지만, 정규 표현식은 더 복잡한 패턴과 조건을 적용할 수도 있습니다. 다양한 메타 문자와 특수 문자를 사용하여 패턴을 정의할 수 있으며, 문자열 대신 변수를 사용할 수도 있습니다.

# 깊이 파고들기

정규 표현식을 좀 더 자세히 알아보겠습니다. 정규 표현식은 문자열과 패턴을 매칭하는 과정에서 여러 가지 옵션과 플래그를 사용할 수 있습니다. 예를 들어, "i" 플래그를 사용하면 대소문자를 구분하지 않고 패턴을 검색할 수 있고, "g" 플래그를 사용하면 문자열 내에서 모든 패턴을 검색할 수 있습니다.

또한 정규 표현식 내에서 여러 가지 메타 문자를 조합해서 사용할 수도 있습니다. 예를 들어, [ ]를 사용하면 여러 문자 중 하나를 선택할 수 있고, ^를 사용하면 해당 문자를 제외할 수 있습니다.

더 자세한 내용은 다음의 링크를 참고해주세요:

- [정규 표현식 입문자를 위한 기초 지식](https://developer.mozilla.org/ko/docs/Web/JavaScript/Guide/정규_표현식#특수_문자)
- [JavaScript의 정규 표현식 문법](https://developer.mozilla.org/ko/docs/Web/JavaScript/Guide/정규_표현식)
- [정규 표현식 실습 사이트](https://regexr.com/)

# 또 다른 방법

- [정규 표현식 연습 문제](https://regexone.com/)
- [정규 표현식 연습 문제와 해답](https://www.tutorialspoint.com/execute_regular_expression_online.php)
- [정규 표현식으로 주어진 문자열 가공하기](https://www.sitepoint.com/regular-expressions-javascript/)
- [정규 표현식으로 문자열 검색하기](https://codeburst.io/javascript-regular-expressions-explained-by-examples-3d02e1112c56)