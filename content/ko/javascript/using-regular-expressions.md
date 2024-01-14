---
title:                "Javascript: 정규 표현식 사용하기"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜
정규 표현식을 사용하는 이유는 간단합니다. 그것은 문자열에서 패턴을 찾고 추출하는 강력한 도구입니다.

## 사용 방법
정규 표현식을 이용하면 문자열에서 원하는 패턴을 쉽게 찾을 수 있습니다. 아래의 예시를 보면서 실제로 어떻게 사용하는지 살펴보겠습니다.

```Javascript
// 이메일 형식 검사
const email = "example@example.com";

if(email.match(/^[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,}$/i)) {
  console.log("유효한 이메일 주소입니다.");
} else {
  console.log("이메일 형식이 올바르지 않습니다.");
}
```

위의 코드에서는 `match()` 메소드를 이용해서 정규 표현식에 해당하는 패턴을 검사합니다. 만약 이메일이 올바른 형식이라면 `유효한 이메일 주소입니다.`가 출력될 것이고, 그렇지 않다면 `이메일 형식이 올바르지 않습니다.`가 출력됩니다.

또 다른 예시로는 문자열에서 숫자를 찾는 경우를 살펴보겠습니다.

```Javascript
// 문자열에서 숫자 찾기
const str = "abc123def";

const numbers = str.match(/[0-9]+/g);

console.log(numbers); // 출력 결과: ["123"]
```

위의 코드에서는 `match()` 메소드와 정규 표현식을 이용하여 문자열에서 숫자만 추출합니다. `g` 플래그를 이용하면 전체 문자열에서 모든 숫자를 추출할 수 있습니다.

## 깊이 파고들기
정규 표현식은 패턴을 정의하고 검색, 치환, 추출 등 다양한 기능을 수행할 수 있습니다. 하지만 정규 표현식은 기초적인 문법부터 복잡한 패턴까지 다양한 사용법이 있기 때문에 학습에 시간이 걸릴 수 있습니다.

정규 표현식을 구성하는 주요 요소는 다음과 같습니다.

- 패턴: 검색할 문자열의 패턴을 정의하는 부분입니다. 예를 들어 `[0-9]`는 숫자를 나타내는 패턴입니다.
- 플래그: 검색할 문자열의 속성을 지정하여 작동 방식을 조절하는 부분입니다. 예를 들어 `g` 플래그는 검색할 문자열 전체에서 패턴을 찾는 것을 의미합니다.
- 메소드: 정규 표현식을 이용하여 패턴을 검색, 치환 등 다양한 작업을 수행할 수 있도록 다양한 메소드를 제공합니다. 예를 들어 `match()` 메소드는 문자열에서 패턴을 검색하는 기능을 수행합니다.

더 자세한 내용은 다음 링크들을 참고하시기 바랍니다.

## 참고 자료
- [MDN 웹 문서 - 정규 표현식](https://developer.mozilla.org/ko/docs/Web/JavaScript/Guide/Regular_Expressions)
- [정규 표현식 30분만에 제대로 배우기](https://github.com/ziishaned/learn-regex/blob/master/translations/README-ko.md)
- [정규 표현식 코너 - 한글 버전](http://okjsp.net:8080/forums/498993)

---
## 참조
-[Markdown 문서로서 포스팅 할 때](http://blog.hwan.sk/2009/11/12/markdown-%EB%AC%B8%EC%84%9C%