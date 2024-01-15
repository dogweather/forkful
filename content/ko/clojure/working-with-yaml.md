---
title:                "yaml과 함께 일하기"
html_title:           "Clojure: yaml과 함께 일하기"
simple_title:         "yaml과 함께 일하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

## 왜?

YAML은 기계가 읽고 쓰기 쉽도록 구조화된 데이터 포맷입니다. 여러분은 많은 프로그래밍 언어에서 YAML을 지원하기 때문에 유용하게 활용할 수 있습니다. 

## 코드로 배우는 방법

```Clojure
;; yaml 라이브러리 임포트
(require '[yaml :as y])

;; YAML 파일 읽기
(y/read-string "name: John
age: 25")

;; 출력: {"name" "John", "age" 25}

;; YAML 파일 쓰기
(y/write-string {"pets": ["dog", "cat", "bird"]})

;; 출력: pets:
;;  - dog
;;  - cat
;;  - bird
```

## 깊이 파고들기

YAML은 들여쓰기를 사용하여 구조를 나타내기 때문에 가독성이 높고 JSON보다 더 유연한 포맷입니다. 여러분은 Clojure에서 yaml 라이브러리를 사용하여 YAML 데이터를 손쉽게 다룰 수 있습니다. YAML은 데이터를 쉽게 읽고 쓰는데 도움이 될 것입니다.

## 더 알아보기

[공식 Clojure yaml 라이브러리 문서](https://clojure.github.io/yaml/)  
[YAML 문법 사용 예제](https://rollout.io/blog/yaml-tutorial-everything-you-need-get-started/)  
[Clojure로 YAML 파싱하기](https://www.programming-books.io/essential/clojure-yaml-parsing-48e49a0f8bc04063bef282f0b024e4cb)