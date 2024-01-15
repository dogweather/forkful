---
title:                "디버그 출력 프린트"
html_title:           "Clojure: 디버그 출력 프린트"
simple_title:         "디버그 출력 프린트"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## 왜

디버그 출력을 하기 위해 노력하는 이유는 단 하나입니다 - 오류를 찾고 고치기 위해서입니다. 디버그 출력은 코드의 어떤 부분이 실행되는지 알려주어 오류를 파악하고 수정하는 데에 도움을 줍니다.

## 어떻게

디버그 출력을 하는 방법은 매우 간단합니다. 우선, `println` 함수를 사용하여 원하는 부분에 해당하는 값을 출력합니다. 예를 들어, 만약 `(+ 1 2)` 값을 디버그하고 싶다면 다음과 같이 작성할 수 있습니다:

```Clojure
(println (+ 1 2))
```

이렇게 하면 출력으로 `3`을 얻을 수 있습니다. 그리고 이를 통해 오류를 파악하고 수정하여 원하는 결과를 얻을 수 있게 됩니다. 

## 딥 다이브

디버그 출력은 매우 유용하지만 너무 많이 사용하면 코드에 불필요한 라인이 늘어나 코드를 읽기 어려울 수 있습니다. 따라서, 디버그 출력은 잘 사용하는 것이 중요합니다. 일반적으로 디버그 출력을 할 때, 다음과 같은 몇 가지 지침을 따르는 것이 좋습니다:

- 오류가 발생하는 부분에만 디버그 출력을 추가합니다.
- 디버그 출력의 내용을 명확하고 간결하게 작성합니다. 
- 디버그 출력을 사용한 후에는 꼭 제거합니다.

위 지침을 따른다면 디버그 출력이 코드에 더 많은 가치를 더할 수 있습니다.

## 이어보기

- [Clojure 공식 문서](https://clojure.org/guides/learn/functions)
- [Clojure 웹사이트](https://clojure.org/)
- [Clojure Cookbook](https://github.com/clojure-cookbook/clojure-cookbook)