---
date: 2024-01-20 17:57:27.683223-07:00
description: "\uD14D\uC2A4\uD2B8 \uAC80\uC0C9 \uBC0F \uAD50\uCCB4\uB294 \uBB38\uC790\
  \uC5F4\uC5D0\uC11C \uD2B9\uC815 \uB2E8\uC5B4\uB098 \uD328\uD134\uC744 \uCC3E\uC544\
  \ \uB2E4\uB978 \uAC83\uC73C\uB85C \uBC14\uAFB8\uB294 \uACFC\uC815\uC785\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uB370\uC774\uD130 \uC815\uB9AC, \uC790\
  \uB3D9\uD654\uB41C \uC218\uC815 \uC791\uC5C5, \uCF54\uB4DC \uB9AC\uD329\uD1A0\uB9C1\
  \ \uB4F1\uC5D0 \uC774\uB97C \uD65C\uC6A9\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:54.639288-06:00'
model: gpt-4-1106-preview
summary: "\uD14D\uC2A4\uD2B8 \uAC80\uC0C9 \uBC0F \uAD50\uCCB4\uB294 \uBB38\uC790\uC5F4\
  \uC5D0\uC11C \uD2B9\uC815 \uB2E8\uC5B4\uB098 \uD328\uD134\uC744 \uCC3E\uC544 \uB2E4\
  \uB978 \uAC83\uC73C\uB85C \uBC14\uAFB8\uB294 \uACFC\uC815\uC785\uB2C8\uB2E4. \uD504\
  \uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uB370\uC774\uD130 \uC815\uB9AC, \uC790\uB3D9\
  \uD654\uB41C \uC218\uC815 \uC791\uC5C5, \uCF54\uB4DC \uB9AC\uD329\uD1A0\uB9C1 \uB4F1\
  \uC5D0 \uC774\uB97C \uD65C\uC6A9\uD569\uB2C8\uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uAC80\uC0C9 \uBC0F \uAD50\uCCB4"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
텍스트 검색 및 교체는 문자열에서 특정 단어나 패턴을 찾아 다른 것으로 바꾸는 과정입니다. 프로그래머들은 데이터 정리, 자동화된 수정 작업, 코드 리팩토링 등에 이를 활용합니다.

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
