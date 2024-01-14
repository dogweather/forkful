---
title:                "Javascript: 문자열 연결하기"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜

자바스크립트 프로그래밍을 하다 보면 다양한 문자열을 사용해야 할 때가 있습니다. 여러 문자열을 합쳐야 할 때가 있는데, 이때 문자열을 합치는 것을 "concatenating"이라고 합니다. 이것은 프로그램에서 필요한 데이터를 보다 효율적으로 다룰 수 있게 해줍니다. 

## 어떻게

자바스크립트에서 문자열을 합치는 방법은 간단합니다. 다음 예제 코드를 살펴보세요.

```Javascript
let firstName = "홍";
let lastName = "길동";

let fullName = firstName + " " + lastName;
console.log(fullName);
```

출력은 다음과 같이 나올 것입니다.

```
홍 길동
```

위 예제에서는 "+" 기호를 사용해 두 개의 문자열을 합치는 방식을 사용했습니다. 또는 다음과 같이 함수를 사용해서도 문자열을 합칠 수 있습니다.

```Javascript
let firstName = "박";
let lastName = "철수";

let fullName = firstName.concat(" ", lastName);
console.log(fullName);
```

출력은 마찬가지로 "박 철수"가 될 것입니다.

## 깊이 파고들기

자바스크립트에서 문자열을 합치는 방법은 간단하지만, 내부적으로는 좀 더 복잡한 이야기가 있습니다. 문자열을 합치면서 컴퓨터는 먼저 문자열을 저장할 메모리 공간을 할당합니다. 그 다음에는 합쳐진 문자열을 저장할 새로운 메모리 공간을 할당하고, 기존의 문자열들을 복사해 넣습니다. 따라서 많은 문자열을 합치는 작업은 성능에 영향을 미칠 수 있으므로 최대한 효율적인 로직을 작성하는 것이 중요합니다.

## 관련 링크

[MDN의 문자열 합치기 관련 문서](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
[웹개발자 포럼에서의 문자열 합치기에 대한 토론](https://forum.webdevelopers.kr/t/topic/887)
[자바스크립트를 활용한 문자열 합치기 예제](https://www.daleseo.com/js-string-concat/)