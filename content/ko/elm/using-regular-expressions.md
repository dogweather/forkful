---
title:    "Elm: 정규식 사용하기"
keywords: ["Elm"]
---

{{< edit_this_page >}}

# 왜 지금 정규식을 배우는 게 좋을까?

정규식은 문자열에서 특정한 패턴을 찾는 데에 유용합니다. 예를 들어, 이메일 주소를 찾거나 전화번호를 검증하는 등 다양한 용도로 사용할 수 있습니다. 또한 정규식은 프로그래밍 언어를 많이 사용하는 개발자들에게는 필수적인 도구이기 때문에 배워두면 큰 도움이 될 것입니다.

## 방법: 

정규식을 사용하는 가장 간단한 방법은 `Regex.find` 함수를 사용하는 것입니다. 아래의 예제를 살펴보세요.

```Elm
import Regex

text = "Hello, my name is John. My email is john@example.com."

regex = Regex.fromString "([a-z]+@[a-z]+\\.[a-z]+)"

Regex.find regex text |> Result.map .match

-- 결과: Just "john@example.com"
```

위 예제에서 `Regex.find` 함수는 `Regex` 모듈에 내장된 함수로, 첫 번째 매개변수로는 정규식 객체를, 두 번째 매개변수로는 문자열을 받아 해당 문자열에서 정규식과 일치하는 부분을 찾아냅니다. `Regex.fromString` 함수를 사용하여 문자열로부터 정규식 객체를 만들 수 있습니다. 이때 정규식 문자열은 백슬래시를 이용하여 특수 문자를 escape 해줘야 합니다. 결과는 `Result` 타입이기 때문에 `Result.map` 함수를 사용하여 값을 추출해줘야 합니다.

즉, 정규식을 사용하는 기본적인 방법은 `Regex.find` 함수를 사용하고, `Regex.fromString` 함수를 사용하여 정규식 객체를 만들어주는 것입니다.

## 깊이 들어가기:

정규식은 다양한 방식으로 사용할 수 있습니다. 예를 들어, 정규식에 대한 전체 매칭 결과를 얻고 싶다면 `Regex.findAll` 함수를 사용할 수 있습니다. 또한, 검색 결과를 특정한 형태로 변환해야 할 때에는 `Regex.replace` 함수를 사용할 수 있습니다. 이 외에도 정규식의 옵션에는 `Regex.Advanced` 모듈을 사용하여 디테일하게 설정할 수 있습니다.

더 많은 정보는 [Elm 공식 문서](https://package.elm-lang.org/packages/elm/regex/latest/)를 참고하시기 바랍니다.

# 참조 자료

- [Elm 공식 문서 - 정규식](https://package.elm-lang.org/packages/elm/regex/latest/)
- [MDN 문서 - 정규식](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [정규식 테스트 사이트](https://regexr.com/)