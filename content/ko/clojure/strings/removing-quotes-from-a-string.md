---
date: 2024-01-26 03:39:38.464410-07:00
description: "\uBC29\uBC95: Clojure\uC5D0\uC11C \uBB38\uC790\uC5F4\uC740 \uBD88\uBCC0\
  \uC774\uBBC0\uB85C, \"\uB530\uC634\uD45C \uC81C\uAC70\"\uC5D0 \uB300\uD574 \uC774\
  \uC57C\uAE30\uD560 \uB54C, \uC2E4\uC81C\uB85C\uB294 \uB530\uC634\uD45C\uAC00 \uC5C6\
  \uB294 \uC0C8\uB85C\uC6B4 \uBB38\uC790\uC5F4\uC744 \uC0DD\uC131\uD558\uB294 \uAC83\
  \uC744 \uB9D0\uD569\uB2C8\uB2E4. \uC5EC\uAE30 `clojure.string/replace`\uB97C \uC0AC\
  \uC6A9\uD55C \uBC29\uBC95\uC774 \uC788\uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.643184-06:00'
model: gpt-4-0125-preview
summary: "Clojure\uC5D0\uC11C \uBB38\uC790\uC5F4\uC740 \uBD88\uBCC0\uC774\uBBC0\uB85C\
  , \"\uB530\uC634\uD45C \uC81C\uAC70\"\uC5D0 \uB300\uD574 \uC774\uC57C\uAE30\uD560\
  \ \uB54C, \uC2E4\uC81C\uB85C\uB294 \uB530\uC634\uD45C\uAC00 \uC5C6\uB294 \uC0C8\uB85C\
  \uC6B4 \uBB38\uC790\uC5F4\uC744 \uC0DD\uC131\uD558\uB294 \uAC83\uC744 \uB9D0\uD569\
  \uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C \uC81C\uAC70\uD558\uAE30"
weight: 9
---

## 방법:
Clojure에서 문자열은 불변이므로, "따옴표 제거"에 대해 이야기할 때, 실제로는 따옴표가 없는 새로운 문자열을 생성하는 것을 말합니다. 여기 `clojure.string/replace`를 사용한 방법이 있습니다:

```clojure
(require '[clojure.string :as str])

; 이중 따옴표를 버립시다
(defn remove-double-quotes [s]
  (str/replace s #"\"" ""))

; 그리고 단일 따옴표를 없앱시다
(defn remove-single-quotes [s]
  (str/replace s #"\'" ""))

; 샘플 사용법:
(remove-double-quotes "\"Hello, World!\"") ; => "Hello, World!"
(remove-single-quotes "'Hello, World!'")   ; => "Hello, World!"
```
단일 및 이중 따옴표를 한 번에 처리하고 싶으세요? 이것을 보세요:

```clojure
(defn remove-quotes [s]
  (str/replace s #"[\"\']" ""))

; 샘플 사용법:
(remove-quotes "\"Hello, 'Clojure' World!\"") ; => "Hello, Clojure World!"
```

## 심층 분석
데이터가 아이의 방처럼 지저분했던 옛날에, 문자열에서 따옴표는 텍스트를 나타내는 표준이었습니다. 하지만 컴퓨터 과학이 발전함에 따라, 따옴표는 단순한 텍스트 구분자가 아닌, 프로그래밍 언어에서 문법적 역할을 수행하게 되었습니다.

Clojure는 Lisp 유산을 갖고 있으며, 다른 언어들이 할 수 있는 것처럼 따옴표를 같은 방식으로 사용하지 않습니다. 확실히 문자열을 나타내는 데 사용되지만, 리터럴을 만드는 데에도 특별한 역할을 합니다. 하지만, 문자열에서 따옴표를 제거하는 것은 시간을 초월한 작업입니다.

왜 문자열 끝부분을 그냥 자르지 않나요? 글쎄요, 그것은 마치 지나치게 애정 넘치는 할머니 할아버지 쌍처럼 문자열의 시작과 끝을 항상 껴안고 있는 따옴표를 가정하는 것입니다. 실제 세계의 데이터는 더 지저분합니다. 여기에서 정규 표현식(정규식)이 등장하여, 숨어있는 따옴표를 어디에서든지 찾아낼 수 있게 합니다.

대안이 있나요? 물론이죠, `subs`, `trim`, `triml`, `trimr`를 사용하거나 심지어 자랑하기 위해 트랜스듀서를 사용할 수도 있습니다. 하지만 정규식을 사용한 `replace`는 칼싸움에 광선검을 가져오는 것과 같아서 - 바로 핵심을 찌릅니다.

## 참고 자료
더 많은 Clojure 문자열 처리 정보를 원하시면, 이러한 정보가 도움이 될 것입니다:

- `clojure.string/replace`에 대한 ClojureDocs: https://clojuredocs.org/clojure.string/replace
- Clojure에서의 정규 표현식: https://clojure.org/guides/learn/syntax#_regex
- 문자열 다루기를 위한 자바 상호운용 (결국 Clojure는 JVM 위에서 실행되니까요): https://clojure.org/reference/java_interop#_working_with_strings

따옴표 제거에만 그치지 마세요. Clojure 땅에서 발견될 수 있는 문자열 마법의 전체 세계가 있습니다.
