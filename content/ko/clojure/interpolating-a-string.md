---
title:                "문자열 내삽하기"
html_title:           "Clojure: 문자열 내삽하기"
simple_title:         "문자열 내삽하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
문자열을 보간(interpolate)한다는 것은 무엇일까요? 보간이란 변수를 문자열에 삽입하는 것을 말합니다. 이는 문자열을 보다 동적으로 만들며, 코드를 더욱 깔끔하고 가독성 있게 만들어 줍니다. 따라서 프로그래머들은 문자열 보간을 자주 사용합니다.

## 하는 법:
이제 어떻게 문자열을 보간하는지 알아볼까요? 예를 들어보겠습니다.

```Clojure
(def name "John") 
(str "Hello, my name is " name "!") 
```

출력: "Hello, my name is John!"

위 코드에서, 변수 "name"을 문자열 안에 삽입하여 보간합니다. 변수 이름 앞에 "str"을 붙여 문자열로 변환해줍니다.

## 깊숙히:
보간의 역사는 어떻게 될까요? 다른 대안이 있을까요? 보간은 일부 언어에서는 따옴표안에 "$" 기호를 사용하여 변수를 쉽게 삽입할 수 있도록 도입되었습니다. Clojure에서는 이 기능 대신 "str" 함수를 사용합니다. 그리고 예시에서는 단순히 변수를 문자열에 삽입하고 있지만, 여러 개의 변수를 사용해 복잡한 문자열을 생성할 수도 있습니다.

## 참고:
보간과 관련된 추가 정보를 알고 싶다면 아래 링크들을 확인해보세요.

- https://clojure.org/reference/reader#_syntax_quote
- https://clojure.org/reference/strings