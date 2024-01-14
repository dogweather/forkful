---
title:    "Javascript: 패턴과 일치하는 문자 삭제하기"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜

일상적으로, 우리는 프로그래밍에서 문자열을 다루게 될 때가 많습니다. 때로는 우리가 원하지 않는 문자열이나 문자 패턴을 찾아 삭제해야 할 때가 있습니다. 이번 블로그 포스트에서는 이러한 상황에서 문자 패턴을 찾아서 삭제하는 방법을 알아보겠습니다.

## 어떻게

먼저 우리는 `replace()` 메소드를 사용하여 문자 패턴을 찾고 삭제할 수 있습니다. 이 메소드를 사용하려면 처음에 찾고자 하는 문자 패턴을 정규식으로 변환해야 합니다. 예를 들어, 우리가 마침표와 느낌표를 삭제하고자 한다면, 정규식 `/[!.]/g`를 사용하여 마침표와 느낌표를 찾을 수 있습니다.

```Javascript
let str = "Hello! World.";
let newStr = str.replace(/[!.]/g, "");
console.log(newStr); // 출력 결과: Hello World
```

위의 코드에서 우리는 `replace()` 메소드를 사용하여 `!`와 `.`를 모두 찾아 삭제한 후 새로운 문자열을 반환하도록 했습니다.

## 깊이 파고들기

정규식은 문자 패턴을 찾고 삭제하는 데 매우 강력한 도구입니다. 위에서 설명한 예제에서는 우리가 찾고자 하는 패턴이 간단하지만, 더 복잡한 패턴을 찾는 데에도 정규식을 사용할 수 있습니다. 정규식에 대해 더 자세히 알고 싶다면 [MDN 문서](https://developer.mozilla.org/ko/docs/Web/JavaScript/Guide/Regular_Expressions)를 참조해보세요.

## 또 다른 자료들

- [정규식 입문하기](https://lucas.codesquad.kr/course/masters/regularexpression.html)
- [정규식 실제 사용 예시](https://velog.io/@kim-mac-record/%EC%A0%95%EA%B7%9C%EC%8B%9D-%EC%82%AC%EC%9A%A9-%EC%98%88%EC%8B%9C)
- [정규식 테스트 사이트](https://regexr.com/)

## 더보기

- [MDN 정규식 문서](https://developer.mozilla.org/ko/docs/Web/JavaScript/Guide/Regular_Expressions)