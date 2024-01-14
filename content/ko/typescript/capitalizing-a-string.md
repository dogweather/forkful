---
title:    "TypeScript: 문자열 대문자로 변환하기"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## 왜
문자열의 첫 번째 글자를 대문자로 바꾸는 것의 가치는 무엇일까요? 우리는 이 글에서 TypeScript로 작성한 코드를 통해 그 이유를 알아보겠습니다.

## 어떻게
우리는 `toUpperCase()` 메소드를 사용하여 문자열의 첫 번째 글자를 대문자로 바꿀 수 있습니다. 아래의 예시 코드를 통해 살펴보세요.

```TypeScript
const str = "hello";
const capitalizedStr = str.charAt(0).toUpperCase() + str.slice(1);
console.log(capitalizedStr); // 출력 결과: "Hello"
```

위의 코드에서 우리는 `charAt()`과 `toUpperCase()` 메소드를 사용하여 첫 번째 글자를 대문자로 바꾸고 `slice()` 메소드를 통해 나머지 문자열을 잘라 다시 합쳤습니다.

## 깊게 들어가보기
문자열의 첫 번째 글자를 대문자로 바꾸는 것은 자주 사용되는 기능입니다. 그렇지만 우리는 `charAt()`과 `toUpperCase()` 외에도 `replace()` 메소드를 사용하여 더 간단하게 처리할 수 있습니다. 아래의 예시 코드를 통해 살펴보세요.

```TypeScript
const str = "hello";
const capitalizedStr = str.replace(/^\w/, c => c.toUpperCase());
console.log(capitalizedStr); // 출력 결과: "Hello"
```

위의 코드에서 우리는 정규표현식(`/^\w/`)을 사용하여 문자열의 첫 번째 글자를 찾고 `toUpperCase()` 메소드를 통해 대문자로 바꿔줍니다. 이렇게 함으로써 더 간단하게 문자열의 첫 번째 글자를 대문자로 바꿀 수 있습니다.

## 관련 자료
- [MDN | String.prototype.toUpperCase()](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [MDN | String.prototype.charAt()](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/charAt)
- [MDN | String.prototype.slice()](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [MDN | String.prototype.replace()](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/replace)