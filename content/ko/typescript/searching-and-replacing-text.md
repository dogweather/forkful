---
title:                "TypeScript: 텍스트 검색 및 교체"
simple_title:         "텍스트 검색 및 교체"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜
텍스트를 검색하고 대체하는 작업을 왜 해야하는지에 대해 알아보겠습니다. 문자열 작업은 모든 프로그래밍 언어에서 매우 중요한 부분이며, 특정 문자열을 무언가 다른 것으로 바꿀 때에도 많이 사용됩니다.

## 방법
우리는 이제 코드 예제와 함께 TypeScript에서 텍스트를 검색하고 대체하는 방법에 대해 살펴보겠습니다. 첫 번째 예제는 간단한 문자열 대체를 보여주고 있습니다.

```TypeScript
let str: string = "안녕하세요, 제 이름은 TypeScript입니다.";
let newStr: string = str.replace("TypeScript", "JavaScript");
console.log(newStr); // 출력 결과: "안녕하세요, 제 이름은 JavaScript입니다."
```

두 번째 예제는 정규식을 사용한 문자열 대체입니다. 정규식을 사용하면 더 복잡한 문자열을 대체할 수 있습니다.

```TypeScript
let str: string = "javascript는 최고의 프로그래밍 언어중 하나입니다.";
let newStr: string = str.replace(/javascript|프로그래밍/gi, (matchedStr) => matchedStr.toUpperCase());
console.log(newStr); // 출력 결과: "JAVASCRIPT는 최고의 프로그래밍 언어중 하나입니다."
```

## 심층 분석
이제 좀 더 깊이 있는 정보를 알아보겠습니다. TypeScript에서 문자열을 검색하고 대체하는 것은 `replace()` 메소드를 사용하는 것입니다. 이 메소드에는 두 개의 인자가 필요합니다. 첫 번째 인자는 대체하려는 문자열 또는 정규식이고, 두 번째 인자는 대체할 새로운 문자열 또는 함수입니다. 또한 `replace()` 메소드를 사용하면 첫 번째 인자로 정규식을 전달하는 것도 가능합니다.

## 또 다른 방법들
자바스크립트에서 문자열을 검색하고 대체하는 방법은 `replace()` 메소드 뿐만 아니라 다양한 방법들이 있습니다. 정규식을 사용한 문자열 대체는 다음 링크를 참고하시기 바랍니다.

- [MDN web docs - replace()](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [MDN web docs - 정규식 시작하기](https://developer.mozilla.org/ko/docs/Web/JavaScript/Guide/Regular_Expressions)
- [정규표현식 체크 및 테스트 사이트](https://regexr.com/)

## 참고 자료
자바스크립트에서 문자열 검색 및 대체에 대한 보다 자세한 내용을 알고 싶다면 다음 링크를 참고하시기 바랍니다.

- [MDN web docs - 문자열 검색 및 대체](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String#Standard_methods)
- [W3Schools - 문자열 대체 메소드](https://www.w3schools.com/js/js_string_methods.asp)
- [TutorialsTeacher - 문자열 검색 및 대체](https://www.tutorialsteacher.com/javascript/javascript-string-replace)