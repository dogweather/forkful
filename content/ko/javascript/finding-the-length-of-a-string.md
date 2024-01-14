---
title:                "Javascript: 문자열의 길이를 찾는 방법"
simple_title:         "문자열의 길이를 찾는 방법"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜
우리는 모두 프로그래밍을 할 때 문자열의 길이를 알아야하는 일이 많습니다. 이 길이를 알려면 문자열의 각 문자를 하나씩 세어야하기 때문에 어려운 작업일 수 있습니다. 하지만 다행히도 자바스크립트에서는 간단한 방법으로 문자열의 길이를 찾을 수 있습니다. 이 글에서는 왜 문자열의 길이를 찾는 것이 중요한지 알아보고, 어떻게 그것을 할 수 있는지 살펴보겠습니다.

## 방법
```Javascript
// 변수에 문자열을 할당합니다.
let str = "안녕하세요";

// 문자열의 길이를 구하는 방법입니다.
let length = str.length;

// 결과는 5가 나옵니다.
console.log(length);
```

위의 예제에서 우리는 `str.length`라는 속성을 사용하여 문자열의 길이를 찾을 수 있습니다. 이 속성은 해당 문자열의 길이를 정수로 반환합니다. 따라서 위의 예제에서 우리는 "안녕하세요"라는 문자열의 길이가 5라는 것을 알 수 있습니다.

또 다른 예제를 살펴보겠습니다.

```Javascript
// 변수에 다른 문자열을 할당합니다.
let str2 = "I love programming";

// 문자열의 길이를 구하는 방법입니다.
let length2 = str2.length;

// 결과는 18이 나옵니다.
console.log(length2);
```

이번에는 `length`라는 이름의 변수를 사용하여 문자열의 길이를 저장한 다음 콘솔에 출력했습니다. 위의 예제에서 우리는 "I love programming"이라는 문자열의 길이가 18이라는 것을 알 수 있습니다.

## 깊이 파해치기
우리는 이미 `str.length`라는 속성을 사용하여 문자열의 길이를 찾는 방법을 배웠습니다. 하지만 이 속성을 사용하는 것 외에도 더 깊이 들어가보겠습니다. 우리는 이미 위의 예제에서 문자열의 길이를 정수로 반환하는 것을 보았기 때문에 이 속성을 사용하면 결과가 정수로 반환된다는 것을 알 수 있습니다. 또한 이 속성을 사용하면 공백도 포함하여 문자열의 길이를 계산한다는 것도 알 수 있습니다.

또 다른 재미있는 사실은 이 속성을 사용하여 배열의 길이를 찾을 수 있다는 것입니다. 우리는 이 블로그 글에서 문자열에 대해 다루고 있지만, 이 속성을 사용하여 배열의 길이도 얻을 수 있습니다. 즉, 배열 안의 항목의 개수를 알 수 있게 됩니다.

## 더보기
- [자바스크립트 문자열 문서](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String)