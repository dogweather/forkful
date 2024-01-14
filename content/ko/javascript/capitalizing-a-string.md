---
title:    "Javascript: 문자열 대문자로 만들기"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열의 첫번째 글자를 대문자로 바꾸는 것은 매우 일반적인 작업입니다. 대문자로 바꾸는 것이 왜 필요한지 궁금하신가요? 그렇다면 계속 읽어보세요!

## 어떻게

자바스크립트에서 문자열의 첫번째 글자를 대문자로 바꾸는 방법은 간단합니다. 우선 `charAt()` 메소드를 사용해 문자열의 첫번째 글자를 선택한 후, `toUpperCase()` 메소드를 이용해 해당 글자를 대문자로 바꿔줍니다.

```Javascript
let str = "hello";
let firstLetter = str.charAt(0).toUpperCase();
console.log(firstLetter);
// Output: H
```

위 코드에서 `str` 변수는 대문자로 바꿀 문자열을, `firstLetter` 변수는 첫번째 글자를 대문자로 바꾼 결과를 저장합니다. `console.log()`는 결과를 출력해줍니다.

그렇다면 여러 글자로 이루어진 문자열의 모든 단어의 첫번째 글자를 대문자로 바꾸려면 어떻게 해야 할까요? 이 때는 `split()` 메소드와 `join()` 메소드를 이용해 각 단어를 분리하고 다시 합쳐줍니다.

```Javascript
let str = "hello world";
let firstLetters = str.split(" ").map(word => word.charAt(0).toUpperCase()).join(" ");
console.log(firstLetters);
// Output: Hello World
```

위 코드에서 `split()` 메소드는 띄어쓰기를 기준으로 단어들을 분리하고, `map()` 메소드를 이용해 각 단어의 첫번째 글자를 대문자로 바꿔줍니다. 마지막으로 `join()` 메소드를 사용해 다시 단어들을 합쳐서 결과를 출력합니다.

## 딥 다이브

문자열의 첫번째 글자를 대문자로 바꾸는 것은 사실 매우 간단한 작업입니다. 하지만 이 작업을 통해 문자열이 가지고 있는 메소드와 개념을 더욱 깊이 이해할 수 있습니다. 대문자와 소문자의 아스키 코드의 차이, 메소드 체이닝 등 다양한 개념을 배울 수 있습니다. 또한 이 작업을 통해 우리가 코드를 작성할 때 더 좋은 방식으로 생각할 수 있게 됩니다.

## 참고 자료

- [MDN Web Docs - String.charAt()](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/charAt)
- [MDN Web Docs - String.toUpperCase()](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [MDN Web Docs - String.split()](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/split)
- [MDN Web Docs - Array.map()](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Array/map)
- [MDN Web Docs - Array.join()](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Array/join)