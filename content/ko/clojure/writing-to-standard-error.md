---
title:                "Clojure: 표준 에러에 쓰는 방법"
simple_title:         "표준 에러에 쓰는 방법"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜?

코드를 작성하다보면 가끔 오류가 발생하게 됩니다. 그리고 우리는 이를 분석하고 수정하기 위해 많은 노력을 기울입니다. 하지만 그럴 때마다 오류 메시지를 복사하여 붙여넣고 분석하는 것은 매우 번거롭고 시간도 많이 듭니다. 여기서 standard error에 쓰는 기술이 유용합니다. 이를 통해 오류 메시지를 쉽게 확인하고 고치는데 도움을 줄 수 있습니다.

## 어떻게?

이 기술을 사용하는 방법을 알아보겠습니다. 먼저 `slurp` 함수를 사용하여 오류가 있는 파일을 읽으면서 아래와 같이 코드를 작성해봅시다.

```Clojure
(require '[clojure.java.io :as io])

(slurp "my_file.txt")
```

위 코드를 실행하면 오류가 발생합니다. 여기서 우리가 필요한 것은 오류 메시지입니다. 이를 확인하기 위해서는 다음과 같이 변경해주어야 합니다.

```Clojure
(require '[clojure.java.io :as io])

(slurp "my_file.txt" {:print-out-out :error})
```

위 코드에서 우리는 `:print-out-out` 키워드를 통해 오류 메시지를 출력하도록 설정해주었습니다. 이제 코드를 다시 실행하면 오류 메시지가 표시됩니다. 이를 통해 우리는 쉽게 오류를 찾고 수정할 수 있습니다.

## 깊게 들어가보기

오류 메시지를 확인하는 데는 여러 가지 방법이 있습니다. 하지만 표준 오류 스트림을 사용하는 것은 가장 간편하고 효과적인 방법입니다. 또한 `slurp` 함수를 이용하여 파일을 읽는 것 뿐만 아니라, 다른 내장 함수들에서도 유용하게 사용할 수 있습니다. 예를 들어, `try-catch` 구문을 사용할 때도 마찬가지입니다. 오류 메시지를 표시하면서 코드를 실행할 수 있습니다. 이를 통해 오류를 더 빠르게 수정할 수 있습니다.

## 또 다른 자료

- [Clojure 공식 문서](https://clojure.org)
- [효과적인 프로그래밍을 위한 Clojure 팁](https://purelyfunctional.tv/guide/effective-clojure/)
- [많은 Java 기반 프로젝트들에서 Clojure를 사용하는 사례들](https://blog.jetbrains.com/idea/2020/08/clojure/)

## 더 보기

위의 자료들을 참고하여 Clojure의 표준 오류 스트림 기술을 더 깊게 이해해보세요. 이를 통해 보다 효율적이고 간결한 코드를 작성할 수 있습니다. 다음 번 코딩할 때 이 기술을 활용해보세요.