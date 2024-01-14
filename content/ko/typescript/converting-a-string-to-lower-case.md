---
title:                "TypeScript: 문자열을 소문자로 변환하기"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

여러분은 TypeScript로 프로그래밍을 하다가 문자열을 소문자로 변환해야 할 때가 있습니다. 왜 매번 문자열을 소문자로 변환할까요? 그 이유를 알아볼까요?

## 왜

대부분의 프로그래밍 언어에서는 대소문자를 구별하므로 문자열을 소문자로 변환하는 것은 중요합니다. 예를 들어 사용자의 입력을 받는 폼에서 이메일 주소를 입력받을 때, 대소문자를 구별하지 않고 비교하려면 문자열을 모두 소문자로 변환해야 합니다. 또한 데이터베이스나 파일 시스템에서도 대소문자를 구별하는 경우가 있으므로 문자열을 소문자로 변환하는 것은 중요합니다.

## How To

TypeScript에서 문자열을 소문자로 변환하는 방법은 간단합니다. 다음 예제를 보며 따라 해보세요.

```TypeScript
let str: string = "HELLO WORLD";
let lowerCaseStr = str.toLowerCase();
console.log(lowerCaseStr); // output: hello world
```

위 예제에서는 `toLowerCase()` 메소드를 사용하여 문자열을 소문자로 변환했습니다. 이 메소드는 문자열의 모든 문자를 소문자로 바꿔줍니다. 따라서 `HELLO WORLD`가 `hello world`로 변환된 것을 볼 수 있습니다.

또 다른 방법으로는 정규표현식을 사용하는 것입니다. 정규표현식을 사용하면 대문자와 소문자를 구분하지 않고 원하는 패턴을 일치시킬 수 있습니다.

```TypeScript
let str: string = "Hello World";
let lowerCaseStr = str.replace(/[A-Z]/g, (match) => match.toLowerCase());
console.log(lowerCaseStr); // output: hello world
```

위 예제에서는 `replace()` 메소드를 사용해 `Hello World` 문자열에서 대문자를 찾아 소문자로 바꾸는 것을 뜻하는 정규표현식을 사용했습니다. 따라서 `Hello World`가 `hello world`로 변환된 것을 볼 수 있습니다.

## Deep Dive

TypeScript에서 문자열을 소문자로 변환하는 방법에는 다양한 메소드가 있습니다. `toLowerCase()` 외에도 `toLocaleLowerCase()`와 `String.prototype.toLocaleLowerCase()` 메소드를 사용할 수 있습니다. 이들 메소드는 인자로 지역 설정을 받아 해당 지역 설정에 맞는 소문자를 반환합니다.

또한 정규표현식을 사용하면 대소문자를 구분하지 않고 일치하는 패턴이 있는 문자열을 소문자로 바꿔줄 수 있습니다. 이를 활용해 더 다양한 문자열 변환 기능을 구현할 수 있습니다.

## See Also

- [TypeScript 공식 문서](https://www.typescriptlang.org/docs/handbook/utility-types.html#lowercasestring-strings)