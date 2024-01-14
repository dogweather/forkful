---
title:                "Clojure: 서브스트링 추출하기"
simple_title:         "서브스트링 추출하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜: 

문자열에서 하위 문자열을 추출하는 것은 개발자들이 자주 사용하는 기술 중 하나입니다. 이를 통해 문자열의 일부분만을 사용하거나 다른 형식으로 변환하여 다양한 용도로 활용할 수 있습니다.

## 어떻게:

다음은 Clojure 프로그래밍에서 하위 문자열 추출의 예시 코드와 결과를 보여줍니다.

```Clojure
;; "Clojure"라는 문자열에서 "oj"만 추출하기
(.substring "Clojure" 2 4)
;; 결과: "oj"

;; 특정 위치부터 끝까지의 문자열 추출
(.substring "Programming" 5)
;; 결과: "mming"

;; 문자열 분할 후 추출
(def string "Breaking news: this is a headline!")
(-> string .split " ")
;; 결과: ["Breaking" "news:" "this" "is" "a" "headline!"]
(nth (.split string " ") 3)
;; 결과: "is"

```

## 딥 다이브:

하위 문자열 추출을 할 때 주의해야 할 점은 시작과 끝 위치를 정확하게 지정하는 것입니다. 만약 시작 위치가 끝 위치보다 큰 경우 빈 문자열을 반환하게 됩니다. 추가적으로, Clojure에서는 문자열 추출을 위해 `.substring` 함수뿐만 아니라 `subs` 함수도 사용할 수 있습니다.

## 더 알아보기:

[Clojure 문자열 추출 문서](https://clojuredocs.org/clojure.core/substring) 

[라이브 데모를 통한 실습](https://mybinder.org/v2/gh/clojurians-org/codox/master?filepath=codox-demo.ipynb)

## 참고 자료: 

[Clojure 패스트 캠프](https://fastcampus.co.kr/class_clojure/#) 

[Clojure 한국 사용자 그룹](https://clojure.or.kr/)