---
title:                "문자열을 소문자로 변환하기"
html_title:           "Clojure: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜

우리는 때때로 문자열을 모두 소문자로 변환해야 할 때가 있습니다. 그것이 우리가 찾는 단어를 더 쉽게 찾도록 돕기 위함인지, 데이터를 일관되게 표시하기 위함인지, 혹은 다른 이유일지 모릅니다. 당신이 자신에게 맞는 이유를 말하기 전까지는, 우리는 그런 작업을 왜 해야 하는지에 대해 생각해볼 수 있습니다.

## 하기

우리가 Clojure를 사용해서 문자열을 소문자로 변환하는 방법을 알아봅시다. 그 방법은 간단합니다. 이러한 작업을 위해 내장된 함수 `lower-case`를 사용하면 됩니다.

```Clojure
(lower-case "HELLO WORLD")
```

출력은 다음과 같습니다:

```
"hello world"
```

위의 예시에서 볼 수 있듯이, 함수 `lower-case`는 문자열을 전달하면 그것을 모두 소문자로 변환해줍니다. 간단하죠? 그런데, 만약 우리가 벡터에 있는 모든 문자열을 소문자로 변환하려면 어떻게 해야 할까요? 다음과 같이 `map` 함수를 사용하면 됩니다:

```Clojure
(map lower-case ["HELLO WORLD" "BYE WORLD"])
```

출력은 다음과 같습니다:

```
("hello world" "bye world")
```

이렇게 `map` 함수를 사용하면 여러 개의 값에 대해서도 소문자로 변환할 수 있습니다.

## 깊게 들어가보기

어떻게 문자열을 소문자로 변환하는지 알아봤습니다. 지금부터는 함수 `lower-case`가 어떻게 작동하는지 조금 더 깊게 들어가보겠습니다.

`lower-case` 함수는 인자로 문자열을 하나 받고, 그 문자열을 소문자로 변환한 새로운 문자열을 반환합니다. 그러나, 이 함수의 인자는 하나의 문자열이 아니라 문자열이 들어있는 구조체나 컬렉션, 혹은 여러 개의 문자열들이 들어있는 경우에도 동작합니다. 그렇기 때문에 위에서 살펴본 예시에서 `map` 함수를 사용해서 벡터 안에 있는 여러 개의 문자열을 소문자로 변환한 것이 가능한 것입니다.

## 참고

- [Clojure 공식 문서](https://clojuredocs.org/clojure.core/lower-case)
- [Clojure 함수의 중첩 사용 예제](https://www.baeldung.com/clojure-nested-functions)
- [Clojure 기초 강좌](https://code.tutsplus.com/kr/tutorials/an-introduction-to-clojure--cms-27482)