---
title:                "텍스트 검색 및 대체"
html_title:           "Clojure: 텍스트 검색 및 대체"
simple_title:         "텍스트 검색 및 대체"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# 무엇 & 왜?: 
검색 및 텍스트 대체는 텍스트를 찾아서 새로운 텍스트로 교체하는 프로그래밍 작업입니다. 이 작업을 하는 이유는 보통 특정 텍스트를 더 쉽게 찾고 교체하기 위해서입니다. 프로그래머들은 이를 통해 코드를 더욱 효율적으로 만들 수 있고 오류를 수정할 수 있습니다.

## 방법:
```Clojure
;; 텍스트 검색하기:
(str/replace "Hello World" #"[A-Z]" "C") 
;; -> "Cello World"

;; 텍스트 대체하기: 
(str/replace "How are you doing?" #"(how)" "How is") 
;; -> "How is are you doing?"
```

## 깊이 있는 정보:
(1) 검색 및 텍스트 대체의 역사적 배경, (2) 대체 방법에 대한 대안들, (3) 구현 세부 사항과 같은 깊은 정보를 알아보겠습니다. 

검색 및 텍스트 대체는 많은 프로그래밍 언어에서 기본적으로 제공되는 기능입니다. Clojure에서는 `str/replace` 함수를 사용하여 문자열에서 원하는 텍스트를 찾고 대체할 수 있습니다. 대안으로, 정규식 외에도 자연어 처리 기술을 사용하여 텍스트를 더욱 정교하게 처리할 수 있습니다. 하지만 이는 복잡성과 성능 문제를 야기할 수 있습니다. 따라서 정규식을 사용하는 것이 일반적입니다.

## 관련 자료:
- [Clojure 공식 문서](https://clojure.org/)
- [정규식 테스트 사이트](https://regexr.com/)