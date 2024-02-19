---
aliases:
- /ko/clojure/removing-quotes-from-a-string/
date: 2024-01-26 03:39:38.464410-07:00
description: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C\uB97C \uC81C\uAC70\
  \uD558\uB294 \uAC83\uC740 \uD14D\uC2A4\uD2B8\uB97C \uAC10\uC2F8\uB294 \uADF8 \uC131\
  \uAC00\uC2E0 \uC774\uC911 \uB610\uB294 \uB2E8\uC77C \uB530\uC634\uD45C \uBB38\uC790\
  \uB97C \uC5C6\uC560\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4. \uD504\uB85C\
  \uADF8\uB798\uBA38\uB4E4\uC740 \uC774 \uC791\uC5C5\uC744 \uB370\uC774\uD130\uB97C\
  \ \uC815\uD654\uD558\uACE0, \uC77C\uAD00\uC131\uC744 \uBCF4\uC7A5\uD558\uBA70, \uB530\
  \uC634\uD45C\uAC00 \uBC14\uB78C\uC9C1\uD558\uC9C0 \uC54A\uAC70\uB098 \uC624\uB958\
  \uB97C \uBC1C\uC0DD\uC2DC\uD0AC \uC218 \uC788\uB294 \uCC98\uB9AC\uB97C \uC704\uD574\
  \ \uBB38\uC790\uC5F4\uC744 \uC900\uBE44\uD558\uAE30 \uC704\uD574\u2026"
lastmod: 2024-02-18 23:09:05.671700
model: gpt-4-0125-preview
summary: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C\uB97C \uC81C\uAC70\uD558\
  \uB294 \uAC83\uC740 \uD14D\uC2A4\uD2B8\uB97C \uAC10\uC2F8\uB294 \uADF8 \uC131\uAC00\
  \uC2E0 \uC774\uC911 \uB610\uB294 \uB2E8\uC77C \uB530\uC634\uD45C \uBB38\uC790\uB97C\
  \ \uC5C6\uC560\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\
  \uB798\uBA38\uB4E4\uC740 \uC774 \uC791\uC5C5\uC744 \uB370\uC774\uD130\uB97C \uC815\
  \uD654\uD558\uACE0, \uC77C\uAD00\uC131\uC744 \uBCF4\uC7A5\uD558\uBA70, \uB530\uC634\
  \uD45C\uAC00 \uBC14\uB78C\uC9C1\uD558\uC9C0 \uC54A\uAC70\uB098 \uC624\uB958\uB97C\
  \ \uBC1C\uC0DD\uC2DC\uD0AC \uC218 \uC788\uB294 \uCC98\uB9AC\uB97C \uC704\uD574 \uBB38\
  \uC790\uC5F4\uC744 \uC900\uBE44\uD558\uAE30 \uC704\uD574\u2026"
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C \uC81C\uAC70\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
문자열에서 따옴표를 제거하는 것은 텍스트를 감싸는 그 성가신 이중 또는 단일 따옴표 문자를 없애는 것을 의미합니다. 프로그래머들은 이 작업을 데이터를 정화하고, 일관성을 보장하며, 따옴표가 바람직하지 않거나 오류를 발생시킬 수 있는 처리를 위해 문자열을 준비하기 위해 수행합니다.

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
