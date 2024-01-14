---
title:                "Clojure: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 읽는 것에 대해 궁금하신가요? 프로그래밍에서 텍스트 파일을 읽는 것은 다양한 정보를 컴퓨터로 가져오는 중요한 일입니다. 이를 통해 여러분은 더 많은 데이터를 제어하고 활용할 수 있게 됩니다.

## 사용 방법

파일을 읽는 법을 알아보기 위해서는 먼저 파일을 열어야 합니다. 이 과정은 몇 가지 단계로 이뤄집니다. 먼저, `with-open` 함수를 사용하여 파일을 열어야 합니다. 그리고 `reader` 함수를 사용하여 파일을 읽을 준비를 해야 합니다. 아래 예시 코드를 참고해보세요.

```
(clojure.pprint/pprint "파일을 열기 위해 with-open 사용하기")

(with-open [file (io/reader "sample.txt")]
  (let [line (line-seq file)]
    (doseq [l line]
      (println l))))
```

위 코드를 실행하면 `sample.txt` 파일의 내용이 각 줄마다 출력됩니다. 이를 통해 파일을 읽을 수 있는 기초적인 방법을 알게 되었습니다.

## 깊이 파고들기

파일을 읽는 방법은 여러 가지가 있습니다. 위의 예시 코드는 파일의 각 줄을 하나씩 읽어오는 방식이었습니다. 하지만 여러분은 `slurp` 함수를 사용하여 파일 전체의 내용을 문자열 형태로 가져올 수도 있습니다. 또한 Clojure는 다양한 라이브러리를 제공하여 파일 형식에 따라 더 쉽게 읽고 처리할 수 있도록 도와줍니다.

## 또 다른 정보들

- [Clojure의 파일 입출력](https://clojure.org/reference/java_interop#_file_io)
- [with-open을 사용한 파일 열기](https://clojuredocs.org/clojure.core/with-open)
- [주요 Clojure 라이브러리](https://clojuredocs.org/libraries)