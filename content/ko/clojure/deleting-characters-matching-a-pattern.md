---
title:    "Clojure: 패턴과 일치하는 문자 삭제하기"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

# 왜

캐릭터 패턴에 일치하는 문자를 삭제하는 것에 대해 의미있는 웹 페이지를 수동으로 검색하고 일일이 삭제하는 것에 비해 더 효율적인 컴퓨터 프로그래밍 방식을 사용하여 시간과 노력을 절약할 수 있습니다.

## 어떻게

```Clojure
;; 캐릭터 패턴 맞는 문자 삭제 함수 정의
(defn delete-pattern [input pattern]
  (apply str (remove #(= % pattern) input)))

;; 예시
(delete-pattern "hello hello hello" \h)
;; 결과: "ello ello ello"

;; 예시
(delete-pattern "abcabcabc" "ab")
;; 결과: "ccc"
```

## 깊은 탐구

캐릭터 패턴 맞는 문자 삭제는 마크다운 포맷에서 제목이나 특정 문자열을 제거하기 위해 일반적으로 사용되는 기능입니다. Clojure 뿐만 아니라 다른 언어에서도 이와 같은 방식으로 구현할 수 있습니다. 또한, 입력 데이터가 매우 큰 경우 메모리와 실행 시간에 영향을 주는 최적화 방법도 존재합니다.

# 봐주세요

- [Clojure Code for Regex-based Pattern Matching and Removal](https://gist.github.com/hr-p/8942459)
- [문자열 삭제하기 - ClojureDocs](https://clojuredocs.org/clojure.string/replace)
- [캐릭터 패턴 맞는 문자 삭제 방법 - Clojure Korea](https://clojure.or.kr/lessons/basics/text-processing.html#패턴맞추기)