---
title:    "Clojure: 정규 표현식 사용하기"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜?
정규식을 사용하는 이유는 매우 간단합니다. 정규식은 특정한 패턴을 가진 문자열을 찾는데에 매우 유용합니다. 예를 들어, 이메일 주소나 전화번호와 같은 형식의 데이터를 찾을 때 정규식은 매우 유용하게 사용될 수 있습니다.

## 어떻게?
아래 예제 코드를 통해 정규식을 사용하는 방법을 알아보겠습니다.

```Clojure
; 이메일 주소 패턴을 정의합니다.
(def email-pattern #"\w+@[\w.]+")

; 정규식과 일치하는 문자열을 찾아 출력합니다.
(print (re-seq email-pattern "이메일 주소는 example@example.com입니다."))

; => (example@example.com)
```

위 코드에서는 `\w`는 알파벳과 숫자를 나타내는 특수 기호이고 `+`는 그것들이 한 번 이상 반복되는 것을 의미합니다. 또한 `@`와 `[\w.]`는 `@` 기호 뒤에 온다는 것이 보장되며, 그 뒤에는 알파벳과 숫자, 그리고 점이 올 수 있다는 것을 의미합니다.

## 심층 분석
정규식의 더 자세한 사용법을 알고 싶다면 다음 링크를 확인해보세요.

- [Clojure 정규식 문서](https://clojuredocs.org/clojure.core/re-seq)
- [정규식 언어로서의 Clojure](https://clojuredocs.org/clojure.string/replace)
- [정규식 빌더](https://regexr.com/)

## 참고 링크
- [정규식 튜토리얼](https://www.regular-expressions.info/tutorial.html)
- [Clojure 공식 문서](https://clojure.org/)
- [정규식 연습 사이트](https://regex101.com/)