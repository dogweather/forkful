---
title:    "TypeScript: 패턴과 일치하는 문자 삭제"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜

얼마 전 나는 새로운 프로젝트를 시작하게 되었습니다. TypeScript를 사용하여 이 프로젝트를 만들기로 결정했는데, TypeScript는 정적 타입을 지원하여 코드를 더 견고하고 유지보수하기 쉽게 만들어줍니다. 프로젝트를 진행하면서, 자바스크립트 문자열에서 특정 패턴을 일치하는 문자를 제거해야 하는 상황이 발생했습니다. 왜 이를 해야 하는지 궁금하셨을 것입니다. 그러므로 오늘은 왜 문자열에서 패턴을 일치하는 문자를 삭제하는 것이 중요한지에 대해 이야기하려고 합니다.

## 어떻게

일치하는 문자를 삭제하는 방법은 간단합니다. 먼저, TypeScript의 `replace()` 메소드를 사용하여 문자열을 새로운 문자열로 대체합니다. 그 다음, 정규식을 사용하여 특정 패턴을 일치하는 문자를 찾아 삭제합니다. 아래의 코드 예제를 참고하세요.

```TypeScript
let str: string = "Hello, world!";
str = str.replace(/l/g, "");
console.log(str); // Heo, word!
```

위의 예제에서, 우리는 `replace()` 메소드를 사용하여 문자열에서 `l`을 모두 찾아 빈 문자열로 대체하였습니다. 그 결과, 문자열에서 모든 `l`이 삭제되었습니다. 정규식을 사용하면 더 복잡한 패턴을 일치하는 문자를 삭제할 수 있습니다.

## 깊이 속 들어가기

여기에서 정규식을 사용하여 문자열에서 특정 패턴을 일치하는 문자를 삭제하는 방법을 알아봤습니다. 그러나 더 깊이 들어가보면, 정규식에 대해 더 많은 것을 배우게 될 것입니다. 이를 통해 더욱 복잡한 문자열 처리를 할 수 있게 됩니다.

## 함께 보기

이 글에서는 문자열에서 패턴을 일치하는 문자를 삭제하는 방법에 대해 배웠습니다. 비슷한 주제로 TypeScript에서 정규식을 사용하는 방법에 대해 더 알고 싶다면 아래의 링크를 참고해보세요.

- [https://www.typescriptlang.org/docs/handbook/regular-expressions.html](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)
- [https://www.regular-expressions.info/tutorial.html](https://www.regular-expressions.info/tutorial.html)