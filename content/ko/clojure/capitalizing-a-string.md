---
title:                "문자열 대문자화"
html_title:           "Clojure: 문자열 대문자화"
simple_title:         "문자열 대문자화"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열 대문자화는 모든 철자를 대문자로 변경하는 과정입니다. 프로그래머들은 가독성을 높이기 위하거나, 문자열의 표준화를 위해 이를 사용합니다.

## 방법은:

Clojure에서 문자열 대문자화는 간단합니다.

```Clojure
(clojure.string/upper-case "Hello, World!")
```
결과 출력:

```Clojure
"HELLO, WORLD!"
```

모든 문자를 대문자로 변경하려면 `clojure.string/upper-case` 함수를 사용하면 됩니다.

## 깊은 이해

문자열 대문자화는 원래 천공 카드를 통해 데이터를 처리할 때 시작됐습니다. 천공 카드는 대문자만 읽을 수 있었기 때문입니다.

Clojure에서 대안으로는 `map` 함수를 이용하여 각 문자를 대문자로 변경할 수 있습니다. 

```Clojure
(apply str (map clojure.string/upper-case "Hello, World!"))
```
하지만, 이 방법은 `clojure.string/upper-case` 함수를 사용하는 것보다 성능이 떨어질 수 있습니다.

## 참고 자료

- [Clojure String API 레퍼런스](https://clojuredocs.org/clojure.string)
- [자바의 String.toUpperCase 메서드를 호출하는 방법](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#toUpperCase())

제목을 작성하거나, 구문 분석, 웹 사이트 URL 등에 일관성을 유지하기 위해 문자열 대문자화를 사용합니다.