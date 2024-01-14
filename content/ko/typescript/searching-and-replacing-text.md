---
title:    "TypeScript: 텍스트 검색 및 교체"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜

자바스크립트 개발자이거나 TypeScript에 익숙한 한국 분들은, 문자열 내부에서의 검색과 대체는 코드 작업 중 매우 중요한 부분입니다. 이를 통해 문자열 내부에 있는 특정 값들을 한 번에 바꿀 수 있고, 코드 작성이 더욱 효율적으로 이루어 집니다.

## 어떻게

먼저, 우리는 `replace()` 메서드를 이용하여 문자열 내부의 값들을 쉽게 바꿀 수 있습니다. 아래 예제를 살펴보세요.

```TypeScript
const str = "안녕하세요! 저는 TypeScript 개발자입니다.";
const newStr = str.replace("TypeScript", "자바스크립트");
console.log(newStr); // "안녕하세요! 저는 자바스크립트 개발자입니다." 
```

해당 메서드는 첫 번째 매개변수로 원하는 값을, 두 번째 매개변수로 바꿀 값을 전달해줍니다. 만약 바꾸고 싶은 모든 값을 한 번에 바꾼다면, 정규표현식을 사용하여 아래와 같은 코드를 작성할 수 있습니다.

```TypeScript
const str = "안녕하세요! TypeScript는 너무 재밌어요. TypeScript를 사용하면서 즐거운 하루 보내세요!";
const newStr = str.replace(/TypeScript/g, "자바스크립트");
console.log(newStr); // "안녕하세요! 자바스크립트는 너무 재밌어요. 자바스크립트를 사용하면서 즐거운 하루 보내세요!"
```

위 예제에서 `/g`는 "global" 옵션을 의미하며, 모든 해당 값들에 일괄적으로 적용됩니다.

## 심화 분석

`replace()` 메서드는 해당 문자열 일부분만 바꾸는 것이 아니라, 전체 문자열에서 모든 해당 값들을 찾아 한 번에 바꾸기 때문에 매우 유용합니다. 또한 정규표현식을 사용하면 더욱 다양한 패턴을 검색하고 대체할 수 있습니다.

## 참고 자료

- [MDN Web Docs | String.replace()](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [MDN Web Docs | 자바스크립트 정규표현식 가이드](https://developer.mozilla.org/ko/docs/Web/JavaScript/Guide/Regular_Expressions)