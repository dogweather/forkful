---
title:                "문자열 연결"
html_title:           "Clojure: 문자열 연결"
simple_title:         "문자열 연결"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
문자열 병합은 여러 개의 문자열을 하나로 합치는 것을 의미합니다. 이를 작성하는 이유는 다양한 데이터를 한 번에 처리하고, 원하는 결과를 도출하기 위함입니다.

## 방법:
```Clojure
(str "Hello" " " "World") ;;결과: "Hello World"
(str "My" (str " " "name") " is John") ;;결과: "My name is John"
```

## 더 들어가기:
1. 문자열 병합은 프로그래밍 언어가 등장하기 이전부터 사용되었으며, 여러 언어에서도 기본적으로 제공됩니다.
2. Clojure에서는 문자열 병합을 위해 `str` 함수를 제공합니다. 하지만, 다른 언어에서는 `+` 기호를 사용하기도 합니다.
3. 문자열 병합은 작업 시간이 오래 걸릴 수 있으므로, 성능 상의 이슈가 있을 수 있습니다.

## 관련 자료:
- Clojure 공식 문서(https://clojure.org/api/cheatsheet)
- 문자열 메소드 참고 자료(https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)