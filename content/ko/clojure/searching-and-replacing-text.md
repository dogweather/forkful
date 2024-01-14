---
title:    "Clojure: 텍스트 검색 및 교체"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜

텍스트를 검색하고 대체하는 것에 관심을 가지는 이유는 여러 가지가 있을 수 있습니다. 예를 들어, 여러분이 작성 중인 프로그램에서 특정 단어나 구를 한 번에 모두 변경하고 싶다면 검색 및 대체 기능은 매우 유용합니다. 또는 엄청난 양의 텍스트를 쉽게 수정하고 싶다면 검색 및 대체 기능을 사용할 수 있습니다.

## 하우 투

검색 및 대체 기능은 Clojure에서 매우 쉽게 사용할 수 있습니다. 먼저 `clojure.string/replace` 함수를 이용하여 특정 문자열을 대체할 수 있습니다. 예를 들어:

```Clojure
(clojure.string/replace "안녕하세요, 세상" "안녕하세요" "안뇽")
```

출력은 "안뇽, 세상"이 됩니다. 문자열 이외에도 정규식을 사용하여 검색 및 대체가 가능합니다. 다음 예제를 확인해보세요:

```Clojure
(clojure.string/replace "abc123def456" #"[0-9]" "")
```

출력은 "abcdef"가 됩니다. 여러 개의 패턴을 동시에 검색하고 대체할 수도 있습니다. 예를 들어:

```Clojure
(clojure.string/replace "안녕하세요, 레드" #{"안녕하세요," "레드"} "안뇽, 블루")
```

출력은 "안뇽, 블루"가 됩니다.이 외에도 `clojure.string/replace-first`, `clojure.string/replace-nth` 등 다양한 함수를 사용하여 검색 및 대체를 할 수 있습니다.

## 딥 다이브

검색 및 대체 기능은 Clojure의 `clojure.string` 네임스페이스에 속해 있습니다. 이 네임스페이스에는 문자열 처리에 유용한 다양한 함수들이 있으니 자세한 내용은 Clojure 공식 문서를 참고하시기 바랍니다.

## See Also

- [Clojure 공식 문서](https://clojure.org/api/cheatsheet)
- [정규식에 관한 자세한 설명](https://www.regular-expressions.info/clojure.html)
- [더 많은 문자열 관련 함수들](https://clojuredocs.org/clojure.string)