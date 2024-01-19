---
title:                "문자열 연결하기"
html_title:           "Arduino: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## 무엇이고 왜 사용하는가?

문자열 연결은 여러 개의 문자열을 하나로 결합하는 작업입니다. 이는 정보를 동적으로 형성하여 사용자에게 보여주거나 데이터를 일관된 형식으로 저장하는 등의 목적으로 프로그래머가 자주 사용합니다.

## 어떻게 사용하는가:

Clojure에서 문자열을 연결하는 가장 간단한 방법은 `str` 함수를 사용하는 것입니다. 또한, 연결할 문자열을 인자로 전달하면 됩니다.

```Clojure
(str "안녕하세요, " "이게 " "문자열 " "연결입니다.")
```

위 코드의 출력:

```Clojure
"안녕하세요, 이게 문자열 연결입니다."
```

## 심화학습

Historically, Clojure designers intended `str` to be the default method for concatenating strings, considering it the most straightforward approach.

In Clojure, there's also the `format` function for string concatenation. Though not as straightforward as the `str` function, `format` gives more control over the final layout of the new string. Think of it as a more powerful version of `str`.

```Clojure
(format "Hello, %s!" "world")
```

The above would result in "Hello, world!".

### 성능 및 구현 상세

저수준에서 볼 때, Clojure의 `str` 함수는 Java의 `StringBuilder`를 사용하여 문자열을 연결합니다. 이는 문자열 연결을 위한 가장 효율적인 방법 중 하나로 알려져 있습니다.

## 참고자료

- [Clojure for the Brave and True: Working with Strings and Characters](http://www.braveclojure.com/core-functions-in-depth/)
- [Clojure Docs: str](https://clojuredocs.org/clojure.core/str)
- [Clojure Docs: format](https://clojuredocs.org/clojure.core/format)