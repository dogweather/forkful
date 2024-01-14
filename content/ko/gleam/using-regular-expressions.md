---
title:    "Gleam: 정규 표현식 사용하기"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜

정규 표현식을 사용하는 이유는 데이터의 정확한 패턴을 찾고 추출하기 위해서입니다.

##어떻게 사용하는가

Gleam에서 정규 표현식을 사용하는 것은 매우 쉽습니다. 다음 코드를 참조해보세요.

```Gleam
import gleam/re
extractor = re.regex("[A-Za-z]+")
result = extractor.match("Hello, world!")
match = case result {
    Ok(value) -> value
    Error(_) -> ""
}
gleam/core/io.info(match) // output: Hello
```

이 예시에서는 정규표현식 "[A-Za-z]+"가 문자열에서 영어 알파벳만을 추출하는데 사용되었습니다. ```re.regex()``` 함수를 사용하여 정규표현식 객체를 생성하고, ```match()``` 함수를 사용하여 해당 정규표현식을 문자열과 매칭하여 결과를 반환합니다.

## 깊게 살펴보기

정규표현식은 다양한 패턴을 표현하는 데 사용되며, 매우 유용한 도구입니다. 그러나 정규표현식은 다소 복잡한 문법을 가지고 있으며, 처음 접하는 사람들에게는 이해하기 어려울 수 있습니다. 정규표현식을 더 깊게 공부하고 싶다면 다음 자료들을 참고해보세요:

- [정규표현식 문법](https://www.w3schools.com/jsref/jsref_obj_regexp.asp)
- [정규표현식 연습용 사이트](https://regexr.com/)
- [Gleam 공식 문서](https://gleam.run/modules/regular_expressions.html)

## See Also
- [Gleam Getting Started Guide](https://gleam.run/getting-started.html)
- [Gleam 공식 문서](https://gleam.run/)