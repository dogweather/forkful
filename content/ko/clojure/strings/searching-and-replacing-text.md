---
date: 2024-01-20 17:57:27.683223-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) \uAC80\uC0C9 \uBC0F\
  \ \uAD50\uCCB4 \uAE30\uB2A5\uC740 \uD14D\uC2A4\uD2B8 \uD3B8\uC9D1\uC758 \uADFC\uAC04\
  \uC774\uBA70, \uC774\uB978\uBC14 'find and replace'\uB294 \uC5D0\uB514\uD130, IDE,\
  \ \uB370\uC774\uD130 \uCC98\uB9AC \uC2A4\uD06C\uB9BD\uD2B8\uC5D0\uC11C \uB110\uB9AC\
  \ \uC0AC\uC6A9\uB429\uB2C8\uB2E4. \uC5ED\uC0AC\uC801\uC73C\uB85C, \uC774 \uAE30\uB2A5\
  \uC740 \uAC1C\uBC1C\uC790\uAC00 \uBC18\uBCF5\uC801\uC778 \uC791\uC5C5\uC744 \uD53C\
  \uD558\uAC8C \uD574\uC8FC\uC5C8\uC2B5\uB2C8\uB2E4.\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:09.117494-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) \uAC80\uC0C9 \uBC0F \uAD50\uCCB4\
  \ \uAE30\uB2A5\uC740 \uD14D\uC2A4\uD2B8 \uD3B8\uC9D1\uC758 \uADFC\uAC04\uC774\uBA70\
  , \uC774\uB978\uBC14 'find and replace'\uB294 \uC5D0\uB514\uD130, IDE, \uB370\uC774\
  \uD130 \uCC98\uB9AC \uC2A4\uD06C\uB9BD\uD2B8\uC5D0\uC11C \uB110\uB9AC \uC0AC\uC6A9\
  \uB429\uB2C8\uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uAC80\uC0C9 \uBC0F \uAD50\uCCB4"
weight: 10
---

## How to: (어떻게 하나요?)
```clojure
; 문자열 내에서 단어를 찾아 교체하기
(defn replace-text [text find replace-with]
  (clojure.string/replace text find replace-with))

; 예제 사용
(replace-text "안녕하세요, 여러분!" "여러분" "Clojure 사용자")
; 출력: "안녕하세요, Clojure 사용자!"
```

```clojure
; 정규 표현식을 사용한 교체
(defn regex-replace [text pattern replace-with]
  (clojure.string/replace text (re-pattern pattern) replace-with))

; 예제 사용
(regex-replace "7 apples, 10 bananas, and 3 pineapples." "\\d+" "#")
; 출력: "# apples, # bananas, and # pineapples."
```

## Deep Dive (심층 분석)
검색 및 교체 기능은 텍스트 편집의 근간이며, 이른바 'find and replace'는 에디터, IDE, 데이터 처리 스크립트에서 널리 사용됩니다. 역사적으로, 이 기능은 개발자가 반복적인 작업을 피하게 해주었습니다. 클로저(Clojure)에서는 `clojure.string/replace` 함수와 정규 표현식을 이용해 이를 간단히 수행할 수 있습니다. 이외에 매크로나 DSL(Domain-Specific Languages)을 활용한 더 복잡하고 강력한 텍스트 변환 방식도 존재합니다. `clojure.string/replace`의 구현은 자바의 `String.replaceAll` 메소드를 사용하여 JVM(Java Virtual Machine) 상에서 실행됩니다.

## See Also (관련 자료)
- 정규 표현식에 대해 더 배우기: [Clojure에서 정규표현식 사용하기](https://clojure.org/guides/learn/functions#_regex)
