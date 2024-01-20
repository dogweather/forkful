---
title:                "정규 표현식 사용하기"
html_title:           "TypeScript: 정규 표현식 사용하기"
simple_title:         "정규 표현식 사용하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

# 무엇 & 왜?

정규식을 사용한다는 것은 문자열에서 패턴을 찾아서 정확한 매칭을 할 수 있게 해주는 것입니다. 프로그래머들은 이를 사용하는 이유는 문자열 처리에 있어서 뛰어난 유연성과 강력한 검색 기능 때문입니다.

# 방법:

TypeScript에서 정규식을 사용하는 방법은 간단합니다. 먼저, `RegExp` 객체를 사용하여 정규식을 생성합니다. 그리고 `test` 또는 `exec` 메서드를 사용하여 문자열에 대한 매칭을 확인하거나 패턴을 추출할 수 있습니다.

```TypeScript
const regex = new RegExp('hello', 'g');
regex.test('hello world'); // true
regex.exec('hey there'); // null
regex.exec('hello world'); // ['hello']
```

`g` 플래그를 사용하면, 전역 매칭을 할 수 있어서 문자열에서 패턴에 해당하는 모든 부분을 찾을 수 있습니다.

```TypeScript
const regex = /hello/g;
const str = 'hello world, hello there';
str.match(regex); // ['hello', 'hello']
```

# 깊이 들어가기:

정규식은 프로그래밍 언어마다 구현 방식이 다를 수 있지만, 보편적으로 `RegExp` 객체를 사용하여 처리됩니다. 과거에는 문자열 처리를 위해 정규식 대신 `substring`과 같은 메서드를 사용하기도 했지만, 정규식의 등장으로 문자열 처리는 크게 향상되었습니다. 정규식은 특수한 패턴을 사용하여 원하는 부분을 정확하게 추출할 수 있기 때문에, 문자열 처리에 있어서 아주 유용합니다.

# 관련 자료:

- [MDN web docs - 정규식](https://developer.mozilla.org/ko/docs/Web/JavaScript/Guide/Regular_Expressions)