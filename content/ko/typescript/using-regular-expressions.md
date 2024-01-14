---
title:    "TypeScript: 정규식 사용하기"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜 regular expressions를 사용하나요?
정규 표현식(regular expressions)은 문자열에서 특정한 패턴을 찾거나 매치시키기 위해 사용되는 매우 강력한 도구입니다. 이 도구를 사용하면 문자열에서 원하는 부분을 추출하거나 검색하고 대치할 수 있으며, 많은 언어에서 지원되는 유용한 기능 중 하나입니다. 따라서 프로그래머들은 정규 표현식을 사용하여 문자열 처리를 간편하고 효율적으로 할 수 있습니다.

## 사용 방법
정규 표현식은 일반적으로 세 가지의 구성 요소로 이루어져 있습니다: 패턴(pattern), 검색 대상 문자열(input), 그리고 플래그(flags). 패턴은 찾고자 하는 문자열의 패턴을 정의하는 부분으로, 이를 통해 문자열에서 일치하는 부분을 찾게 됩니다. 검색 대상 문자열은 실제로 패턴을 찾을 대상 문자열을 의미하며, 대부분 문자열 변수나 문자열 리터럴로 제공됩니다. 마지막으로 플래그는 패턴을 적용할 때 어떤 규칙을 따를지를 정하는 옵션으로, 예를 들어 대소문자를 무시하거나 전체 문자열에서 모든 일치하는 부분을 찾을지를 결정할 수 있습니다.

```TypeScript
// input 문자열에서 "cat"이라는 패턴을 검색하는 예시
let input = "I have a cat and a dog.";
let pattern = /cat/;
console.log(pattern.test(input)); // true
```

위의 예시에서는 input 문자열에서 "cat"이라는 부분을 찾으며, 찾으면 true를 출력합니다.

더욱 복잡한 예시를 살펴보겠습니다.

```TypeScript
// input 문자열에서 모든 전화번호를 추출하는 예시
let input = "My phone number is (123)456-7890. Your number is (987)654-3210.";
let pattern = /\(\d{3}\)\d{3}-\d{4}/g;
console.log(input.match(pattern)); // ["(123)456-7890", "(987)654-3210"]
```

위의 예시에서는 괄호로 묶인 세 자리 숫자 다음에 세 자리 숫자와 하이픈, 그리고 마지막으로 네 자리 숫자가 있는 패턴을 찾는 정규 표현식을 사용하고 있습니다. 이를 통해 input 문자열에서 모든 전화번호를 추출할 수 있습니다.

## 깊이있게 살펴보기
정규 표현식은 강력한 도구지만 사용하는 패턴에 따라 복잡해질 수 있습니다. 또한 잘못된 패턴을 사용하면 의도하지 않은 결과가 나올 수도 있습니다. 따라서 정규 표현식을 사용할 때는 반드시 문서를 참조하고 예제 코드를 참고하여 익숙해지는 것이 좋습니다. 또한 정규 표현식을 이용해 문자열을 처리할 때 각 언어별로 다르게 적용되는 문법이 있으므로, 자신이 사용하는 언어의 문법을 항상 확인하는 것이 중요합니다.

## 더 알아보기
- [MDN 웹 문서: 정규 표현식](https://developer.mozilla.org/ko/docs/Web/JavaScript/Guide/Regular_Expressions)
- [자바스크립트 정규 표현식 테스트기](https://regexr.com/)
- [정규표