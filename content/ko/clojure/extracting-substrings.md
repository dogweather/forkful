---
title:                "부분 문자 추출"
html_title:           "Clojure: 부분 문자 추출"
simple_title:         "부분 문자 추출"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

서브스트링을 추출하는 것은 문자열에서 일부 문자들을 잘라내는 작업입니다. 프로그래머들은 주어진 문자열에서 원하는 부분만을 추출하기 위해 서브스트링을 사용합니다.

## 하는 방법:

```Clojure
(def str "Hello World")

(subs str 0 5) ;output: "Hello"
(subs str 6) ;output: "World"
(subs str 0 6) ;output: "Hello "
```

위의 코드 예제에서 ```subs``` 함수는 문자열에서 주어진 범위 내의 문자들을 잘라낼 수 있도록 해줍니다. 첫 번째 매개변수로는 추출할 문자열을, 두 번째 매개변수로는 추출할 문자열의 시작 위치를, 세 번째 매개변수로는 추출할 문자열의 끝 위치를 전달해 줍니다. 만약 끝 위치를 전달하지 않으면 시작 위치부터 문자열의 끝까지 추출하게 됩니다.

## 깊이 알아보기:

서브스트링 추출은 옛날부터 사용되어 온 방법이며, 다양한 언어에서도 지원해오고 있습니다. 다른 방법으로는 정규표현식을 사용하거나 다른 문자열 함수들을 조합하여 서브스트링을 추출할 수도 있습니다.

서브스트링 추출은 문자열을 가공하는 작업 중 하나로, 문자열을 다루는 프로그램 작성에 있어서 필수적입니다. 이 작업을 잘 활용하면 프로그램의 효율성을 높일 수 있습니다.

## 관련 자료:

정규표현식에 대해 좀 더 자세히 알고 싶다면 [이 링크](https://ko.wikipedia.org/wiki/정규_표현식)를 참고해보세요.

Clojure 관련 자료는 [공식 문서](https://clojuredocs.org/core-library/substring)에서도 확인할 수 있습니다.