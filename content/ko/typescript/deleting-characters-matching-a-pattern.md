---
title:                "TypeScript: 패턴과 일치하는 문자 삭제"
simple_title:         "패턴과 일치하는 문자 삭제"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜?

자바스크립트나 TypeScript를 사용하는 개발자 중에는 때때로 특정 패턴과 일치하는 문자를 삭제하는 작업이 필요한 경우가 있습니다. 이럴 때에는 어떤 상황에서 어떤 방식으로 문자를 삭제해야하는지에 대해 알고 있어야 합니다.

## 삭제 방법

```TypeScript
// 패턴에 일치하는 문자를 삭제하는 함수
function deleteCharacters(str: string, pattern: RegExp) {
    // replace 메소드를 사용하여 패턴과 일치하는 문자를 빈 문자열로 대체
    return str.replace(pattern, "");
}

// 예시
let str = "Hello, TypeScript!";
let pattern = /[aeiou]/g;
let result = deleteCharacters(str, pattern);
console.log(result); // Hll, TypScrpt!
```

위의 코드는 문자열과 정규 표현식을 매개변수로 받아 해당 문자열에서 패턴과 일치하는 문자를 삭제하는 함수를 보여줍니다. 문자열에서 패턴과 일치하는 모든 문자를 삭제하려면 정규 표현식에 `g` 플래그를 추가해야 합니다. 

또한, 다양한 패턴을 사용하여 문자를 삭제할 수 있습니다. 예를 들어, `[aeiou]` 패턴을 사용하면 모음을 삭제할 수 있고, `[0-9]` 패턴을 사용하면 숫자를 삭제할 수 있습니다. 또한 `^` 기호를 사용하여 특정 문자나 숫자를 제외하고 모든 문자를 삭제할 수 있습니다.

## 깊이 파고들기

문자를 삭제하는 방법에 대해 더 깊이 이해하려면 정규 표현식에 대해 더 알아야 합니다. 정규 표현식은 문자열에서 특정 패턴을 찾고 대체하는데 유용한 패턴 매칭 도구입니다. 정규 표현식을 사용하여 문자를 삭제하는 방법 외에도 문자열을 다양한 방식으로 조작할 수 있습니다.

예를 들어, `replace()` 메소드의 두 번째 인수로는 문자열 대신 함수를 전달할 수 있습니다. 이 함수는 매칭된 패턴의 결과를 처리하여 원하는 값으로 대체할 수 있습니다. 이를 통해 정교한 문자열 조작이 가능해집니다.

또한, 정규 표현식의 여러 기능을 활용하면 문자열 내의 패턴을 더 정확하게 찾을 수 있습니다. 예를 들어, `\w` 는 모든 알파벳 문자와 숫자, `_` 기호를 의미하고, `\d` 는 모든 숫자, `\s` 는 공백 문자를 의미합니다. 이렇게 다양한 기능을 활용하여 정확한 패턴을 찾아내는 것이 가능합니다.

## 더 알아보기

- [TypeScript 공식 문서 - 정규 표현식](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)
- [MDN web docs - 정규 표현식](https://developer.mozilla.org/ko/docs/Web/JavaScript/Guide/Regular_Expressions)
- [제타위키 - 정규 표현식](https://zetawiki.com/wiki/%EC%A0%95%EA%B7%9C_%ED%91%9C%ED%98%84%EC%8B%9D)