---
title:                "문자열 대문자 변환"
html_title:           "Clojure: 문자열 대문자 변환"
simple_title:         "문자열 대문자 변환"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# 무엇 & 왜?

문자열을 대문자로 변환하는 것이 무엇인지 설명하고 프로그래머들이 왜 그렇게 하는지 알려줍니다.

## 어떻게:

기존의 문자열을 대문자로 변환하는 코드와 예시 출력을 ```Clojure ... ``` 코드 블록 내에 넣어서 보여줍니다.

```Clojure
;; 스트링 "hello world"를 대문자로 변환하는 예제 코드
; 입력: "hello world"
; 출력: "HELLO WORLD"
(println (clojure.string/upper-case "hello world"))
```

## 깊이 파고들기:

(1) 역사적인 문맥, (2) 대안들, 그리고 (3) 문자열을 대문자로 변환하는 구현 세부사항에 대한 정보와 함께 더 깊게 들어가 봅니다.

## 관련 자료:

문자열을 대문자로 바꾸는 데 관련된 링크들을 소개합니다.

- https://clojuredocs.org/clojure.string/upper-case
- https://www.clojure.org/api/cheatsheet#Upper-case
- https://clojure.org/reference/strings