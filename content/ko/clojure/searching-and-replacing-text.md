---
title:                "Clojure: 텍스트 검색과 바꾸기"
simple_title:         "텍스트 검색과 바꾸기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜 교체를 검색해야 하는가?

교체는 프로그래밍에서 매우 일반적인 작업입니다. 텍스트를 교체함으로써 코드의 일부를 다른 코드로 간단하게 변경할 수 있습니다. 또는 오타를 찾아서 수정하는 데 유용할 수 있습니다. 이번 블로그 포스트에서는 Clojure에서 텍스트를 검색하고 교체하는 방법에 대해 살펴보겠습니다.

## 어떻게 하나요?

Clojure에서 텍스트를 교체하는 가장 간단한 방법은 `str/replace` 함수를 사용하는 것입니다. 예를 들어, 다음과 같이 문자열에서 모든 공백을 제거할 수 있습니다.

```Clojure
(str/replace "Hello world" #" " "")
```

이 코드를 실행하면 결과로 `Helloworld`를 얻을 수 있습니다. `"` 사이의 첫 번째 인자는 교체할 문자열이고, `#" "` 사이의 두 번째 인자는 교체할 패턴입니다. 마지막 `" "`은 `""`으로 교체되므로 공백이 제거됩니다.

만약 모든 숫자를 `x`로 바꾸고 싶다면 이것을 어떻게 할 수 있을까요? `str/replace` 함수를 다음과 같이 변경하면 됩니다.

```Clojure
(str/replace "A1B2C3" #"\d" "x")
```

이를 실행하면 `AxBxCx`라는 결과가 나오게 됩니다. `\d`는 숫자에 해당하는 정규식 표현이며, `x`로 대체됩니다.

또한 `str/replace` 함수는 대소문자를 무시하도록 `:ignore-case` 옵션을 제공합니다. 예를 들어, `"hello"`를 `"HELLO"`로 바꾸고 싶다면 다음과 같이 작성할 수 있습니다.

```Clojure
(str/replace "hello" #"hello" "HELLO" {:ignore-case true})
```

위 코드를 실행하면 `"HELLO"`를 얻게 됩니다.

## 심층 분석

좀 더 깊은 수준에서 텍스트 교체에 대해 이해하기 위해, Clojure의 `clojure.string` 네임스페이스에 있는 몇 가지 함수를 살펴보겠습니다.

### `split` 함수

`split` 함수는 문자열을 분리하는 데 유용합니다. 첫 번째 인자로는 분리할 문자열을, 두 번째 인자로는 활용할 패턴을 전달합니다. 예를 들어, `clojure.string` 네임스페이스를 로드하고 `split` 함수를 사용해보겠습니다.

```Clojure
(ns example.core
  (:require [clojure.string :as str]))

(str/split "Hello world" #"\s")
```

위 코드를 실행하면 결과로 `["Hello", "world"]`를 얻게 됩니다. `\s`는 공백을 나타내는 정규식 표현입니다.

### `join` 함수

`join` 함수는 리스트를 결합하여 문자열을 만드는 데 사용합니다. 예를 들어, 다음과 같이 `join` 함수를 사용해보겠습니다.

```Clojure
(str/join "-" ["a" "b" "c"])
```

위 코드를 실행하면 결과로 `"a-b-c"`를 얻게 됩니다. 첫 번째 인자는 결합할 구분자를, 두 번째 인자는 결합할 리스트를 전달합니다.

## 더 알아보기

위에서 살펴본 것 이외에도 Clojure에서는 다양한 방법으로 텍스트를 검색하고 교체할 수 있습니다.