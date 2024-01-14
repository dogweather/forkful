---
title:    "Clojure: 문자열 소문자로 바꾸기"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 왜
문자열을 소문자로 변환하는 일에 참여하는 이유는 다양합니다. 예를 들어, 데이터를 비교할 때 대소문자를 무시하고 비교하기 위해서이거나, 사용자로부터 입력 받은 데이터를 표준화하기 위해서일 수 있습니다.

# 어떻게
우리는 문자열을 소문자로 변환하는 다양한 방법이 있지만, Clojure에서는 `lower-case` 함수를 사용하여 손쉽게 할 수 있습니다. 다음은 이 함수를 사용한 예시 코드입니다.

```Clojure
(lower-case "HELLO") ; => "hello"
(lower-case "Greetings") ; => "greetings"
```

# 깊이 파고들기
Clojure에서 `lower-case` 함수는 문자열을 소문자로 변환하기 위해 Unicode 규약을 따릅니다. 이 함수를 통해 문자열의 각 문자를 Unicode로 변환한 뒤 소문자로 변환합니다. 또한, `lower-case` 함수는 `locale` 옵션을 통해 현재 언어 설정에 맞게 작동할 수 있도록 할 수 있습니다. 이 기능은 특정 언어에서 대소문자를 다르게 사용하는 경우 유용합니다.

# 관련 링크
- Clojure 문자열 관련 함수들: [https://clojuredocs.org/clojure.core#string](https://clojuredocs.org/clojure.core#string)
- Clojure 공식 사이트: [https://clojure.org/](https://clojure.org/)
- Unicode 규약: [http://unicode.org/standard/standard.html](http://unicode.org/standard/standard.html)

# 참고
- See Also: 관련 링크