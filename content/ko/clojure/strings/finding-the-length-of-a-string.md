---
date: 2024-01-20 17:47:30.078094-07:00
description: "\uBB38\uC790\uC5F4\uC758 \uAE38\uC774\uB97C \uCC3E\uB294 \uAC74 \uADF8\
  \ \uBB38\uC790\uC5F4\uC774 \uBA87 \uAC1C\uC758 \uBB38\uC790\uB97C \uD3EC\uD568\uD558\
  \uB294\uC9C0 \uC138\uB294 \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uBB38\uC790\uC5F4 \uCC98\uB9AC, \uB370\uC774\uD130 \uAC80\uC99D, \uD639\
  \uC740 UI \uBD80\uBD84\uC5D0\uC11C \uBB38\uC790 \uC81C\uD55C\uC744 \uC801\uC6A9\uD560\
  \ \uB54C \uC774\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:54.647513-06:00'
model: gpt-4-1106-preview
summary: "\uBB38\uC790\uC5F4\uC758 \uAE38\uC774\uB97C \uCC3E\uB294 \uAC74 \uADF8 \uBB38\
  \uC790\uC5F4\uC774 \uBA87 \uAC1C\uC758 \uBB38\uC790\uB97C \uD3EC\uD568\uD558\uB294\
  \uC9C0 \uC138\uB294 \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\
  \uC740 \uBB38\uC790\uC5F4 \uCC98\uB9AC, \uB370\uC774\uD130 \uAC80\uC99D, \uD639\uC740\
  \ UI \uBD80\uBD84\uC5D0\uC11C \uBB38\uC790 \uC81C\uD55C\uC744 \uC801\uC6A9\uD560\
  \ \uB54C \uC774\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC758 \uAE38\uC774 \uCC3E\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열의 길이를 찾는 건 그 문자열이 몇 개의 문자를 포함하는지 세는 것입니다. 프로그래머들은 문자열 처리, 데이터 검증, 혹은 UI 부분에서 문자 제한을 적용할 때 이를 사용합니다.

## How to: (방법)
Clojure에서 문자열 길이를 찾는 방법은 `count` 함수나 `.length()`를 사용하는 것입니다. 예시를 보겠습니다:

```Clojure
;; count 함수 사용
(count "안녕하세요") ; => 5

;; .length 활용 (Java interop)
(.length "안녕하세요") ; => 5
```

둘 다 "안녕하세요"라는 문자열의 길이가 5임을 나타냅니다.

## Deep Dive (심층 분석)
Clojure는 Java Virtual Machine (JVM)에 기반한 언어이기 때문에, 자바의 메서드를 직접 호출하거나 Clojure의 내장 함수를 사용할 수 있습니다. `count` 함수는 범용적이어서 컬렉션의 길이를 찾을 때도 사용하지만, `.length()`는 문자열에 특화된 Java 메서드입니다.

왜 둘 중 하나를 선택하냐고요? `.length()`가 조금 더 빠르기는 하지만, `count`는 Clojure스러운 방법입니다. `count`는 다른 시퀀스에도 사용할 수 있어 일관성을 유지할 수 있습니다.

역사적으로 볼 때, `.length()`는 자바 문자열이 제공하는 기본 메서드이며, JVM 위에서 작동하는 언어에서 널리 사용되는 방식입니다. Clojure가 나타나며 함수형 프로그래밍의 패러다임을 가져왔고, `count`와 같은 함수를 제공하여 좀 더 추상화된 방법을 선호하게 되었습니다.

## See Also (관련 정보)
- Clojure 공식 문서의 [`count` 함수](https://clojuredocs.org/clojure.core/count)
- Java 문자열 [`length` 메소드](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#length())
- [Clojure for the Brave and True](https://www.braveclojure.com/) - 루키 프로그래머를 위한 자세한 Clojure 입문서
