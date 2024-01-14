---
title:    "Javascript: 정규 표현식 사용하기"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

# 왜 정규 표현식을 사용하는가?

정규 표현식은 Javascript에서 문자열을 다루면서 가장 강력한 도구 중 하나입니다. 문자열을 처리하고 추출할 수 있도록 유연한 패턴 매칭 기능을 제공합니다. 따라서 특정한 조건의 문자열을 찾거나 수정하는 등 다양한 작업을 할 때 매우 유용합니다.

# 사용 방법

정규 표현식을 사용하기 위해서는 먼저 `RegExp` 객체를 선언해야 합니다. 이 객체는 정규 표현식의 패턴과 플래그를 지정할 수 있으며, 이를 사용해 문자열을 매칭할 수 있습니다. 예를 들어, `RegExp` 객체로 `"hello"`라는 문자열을 찾아보겠습니다.

```Javascript
let string = "hello world";
let regex = new RegExp("hello");
console.log(regex.test(string));
```

위 코드의 결과는 `true`가 출력됩니다. `regex` 객체를 통해 문자열이 `hello`를 포함하고 있는지 여부를 판단할 수 있었습니다. 또한 정규 표현식에서 `g` 플래그를 추가하면 전역에서 `hello`를 찾아서 이를 모두 수정하거나 추출할 수 있습니다.

# 더 알아보기

정규 표현식을 사용할 때 주의할 점이 있습니다. 예를 들어, `.`이라는 메타 문자는 어떤 문자라도 매칭하도록 정의되어 있기 때문에 `"hello"`라는 문자열에서 `.`을 사용하면 `e`와 `o`가 매칭될 것입니다. 이런 메타 문자를 이용한 패턴화를 통해 좀 더 복잡한 문자열 처리가 가능합니다. 또한 정규 표현식의 더 자세한 사용법과 예제는 [여기](https://developer.mozilla.org/ko/docs/Web/JavaScript/Guide/Regular_Expressions)에서 확인할 수 있습니다.

# 참고자료

- [정규 표현식을 알아보자](https://developer.mozilla.org/ko/docs/Web/JavaScript/Guide/Regular_Expressions)
- [정규 표현식 간단 정리](https://medium.com/@jellyms/%EC%A0%95%EA%B7%9C-%ED%91%9C%ED%98%84%EC%8B%9D-%EA%B0%84%EB%8B%A8-%EC%A0%95%EB%A6%AC-regular-expression-546446eba048)
- [참고할 수 있는 정규 표현식 실습 사이트](https://regexr.com/)