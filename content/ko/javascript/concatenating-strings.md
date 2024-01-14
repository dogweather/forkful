---
title:    "Javascript: 문자열 연결하기"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜

문자열을 연결하는 것은 프로그래밍에서 가장 기본적이고 자주 사용되는 기술 중 하나입니다. 문자열을 연결하는 것으로 두 개 이상의 문자열을 하나의 문자열로 결합할 수 있으며, 이를 통해 더 복잡한 프로그래밍 문제를 해결할 수 있습니다. 

## 방법

```Javascript 
let firstName = "용우";
let lastName = "김";
let fullName = firstName + lastName;

console.log(fullName); // 출력 결과: 용우김
```

위의 코드 예제에서는 먼저 firstName 변수와 lastName 변수를 선언하고, 두 변수를 + 연산자를 이용해 fullName 변수에 할당했습니다. 그리고 console.log() 함수를 사용해 fullName 변수를 출력할 수 있습니다. 이 때 출력되는 결과는 "용우김"이 됩니다. 

또한, + 연산자뿐만 아니라 ES6부터는 템플릿 리터럴(Template Literal)을 이용하여 문자열을 연결할 수도 있습니다. 아래 코드 예제를 살펴보세요.

```Javascript 
let firstName = "용우";
let lastName = "김";
let fullName = `${firstName}${lastName}`;

console.log(fullName); // 출력 결과: 용우김
```

여기서 주목해야 할 점은 템플릿 리터럴을 사용하면 자동으로 문자열을 연결해준다는 것입니다. 따로 + 연산자를 사용하지 않아도 됩니다. 

## 깊이 파고들기

문자열을 연결하는 방법은 간단하지만, 실제로는 어떻게 동작하는 지 궁금할 수 있습니다. 자바스크립트에서는 문자열을 연결할 때 메모리 공간을 할당하고, 두 문자열을 결합한 새로운 문자열을 할당합니다. 하지만 이 과정은 매우 빠르게 이루어지기 때문에 실제로는 거의 느낄 수 없는 차이입니다.

또한, 문자열을 연결할 때 주의해야 할 점도 있습니다. 만약 매우 긴 문자열을 연결하거나, 반복문을 사용하여 많은 문자열을 연결하게 되면 성능 문제가 발생할 수 있습니다. 이런 경우에는 배열 또는 다른 방법을 고민해보는 것이 좋습니다. 

## 더 알아보기

[MDN 문서 - 문자열 연결](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/concat)

[MDN 문서 - 템플릿 리터럴](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Template_literals)