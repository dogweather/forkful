---
title:                "정규 표현식 사용하기."
html_title:           "Javascript: 정규 표현식 사용하기."
simple_title:         "정규 표현식 사용하기."
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜 정규 표현식을 사용해야 하나요?

정규 표현식은 문자열을 다루는 강력한 도구입니다. 문자열에서 특정 패턴을 찾거나 변경할 수 있으며, 데이터 유효성 검사 및 언어 분석 등 다양한 용도로 활용할 수 있습니다.
<br>

## 사용 방법

정규 표현식을 사용하는 방법을 알아보겠습니다. 아래 코드 블록에는 문자열을 다루는 기본적인 정규 표현식 예제가 포함되어 있습니다. 이를 통해 정규 표현식이 어떻게 동작하는지 살펴보세요.

```Javascript 
let str = "I love Javascript!";

// 'javascript'라는 단어를 모두 대문자로 변경
let newStr = str.replace(/javascript/gi, "JAVASCRIPT");
console.log(newStr); // I love JAVASCRIPT!
```

위 코드에서 `replace()` 메소드의 첫 번째 매개변수에는 정규 표현식이 전달되고, 두 번째 매개변수에는 변경할 값이 전달됩니다. `gi`는 정규 표현식의 옵션을 나타내며, `g`는 전체 문자열에서 찾고, `i`는 대소문자를 구분하지 않고 찾는다는 의미입니다.

또 다른 예제를 살펴보겠습니다.

```Javascript
let str = "Hello, world!";
let vowels = str.match(/[aeiou]/gi); // 모음을 찾아 배열로 반환
console.log(vowels); // ["e", "o", "o"]
```

위 코드에서는 `match()` 메소드와 정규 표현식을 사용하여 문자열에서 모음을 찾아 배열로 반환합니다. `/[aeiou]/gi`는 `a, e, i, o, u` 중 하나의 문자를 찾으며, `g`와 `i`는 앞서 설명한 옵션과 동일한 역할을 합니다.

또한, 정규 표현식을 사용하여 문자열에서 특정 패턴을 검사할 수도 있습니다. 예를 들어 이메일 주소의 유효성을 검사하는 코드는 다음과 같이 작성할 수 있습니다.

```Javascript
let email = "example@gmail.com";

if(email.match(/^[a-z0-9_]{2,20}@[a-z0-9]{2,10}\.[a-z]{2,3}$/i)){
    console.log("유효한 이메일 주소입니다.");
} else {
    console.log("유효하지 않은 이메일 주소입니다.");
}
```
위 코드에서 `match()` 메소드의 첫 번째 매개변수는 이메일 주소의 패턴을 나타내며, 정규 표현식에 대한 설명은 이 공간에서 다루지 않겠습니다. 하지만 만약 이메일 주소가 해당 패턴에 일치하지 않으면 `유효하지 않은 이메일 주소입니다.`가 출력됩니다.

## 깊이있게 알아보기

정규 표현식은 매우 복잡한 패턴을 나타낼 수 있으며, 이를 활용하면 문자열을 다루는 데 유용합니다. 하지만 정규 표현식을 배우고 이해하는 데는 시간이 걸릴 수 있으며, 실제로 익숙해지려면 연습이 필요합니다. 정규 표현식을 자유자재로 다루고 싶다면 많이 읽고 연습하는 것이 중요합니다. 자세한 내용은 아래의 링크를 참고하세요.

##