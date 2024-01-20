---
title:                "텍스트 파일 읽기"
html_title:           "Bash: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

텍스트 파일 읽기는 컴퓨터 프로그래밍에서 중요한 핵심 기능으로, 실제로 파일의 내용을 가져오는 과정입니다. 이는 데이터를 저장하고 분석하는데 필수적인 기능입니다.

## 어떻게 하나요:

Clojure에서 텍스트 파일을 읽는 가장 기본적인 방법을 알아봅시다. 다음 코드를 확인해 보세요.

```clojure
(with-open [reader (clojure.java.io/reader "file.txt")]
  (doseq [line (line-seq reader)]
    (println line))) 
```

이 코드를 실행하면 "file.txt"라는 텍스트 파일의 모든 줄을 출력합니다.

## 깊이 들여다보기 

Clojure에서 파일을 읽는 방법은 시간이 지남에 따라 발전했습니다. 초기에는 Java의 파일 읽기 메서드를 사용했지만, 이제는 Clojure 자체의 `clojure.java.io/reader`를 사용합니다.

텍스트 파일을 읽는 대안적인 방법으로는 `slurp` 함수가 있습니다. 이 함수는 전체 파일을 하나의 문자열로 읽어들입니다. 그러나 이는 파일이 매우 큰 경우 메모리 문제를 일으킬 수 있으므로 주의해야 합니다.

```
(slurp "file.txt")
```

텍스트 파일 읽기의 중요한 부분은 파일을 열고 닫는 것입니다. `with-open`은 파일을 안전하게 열고 작업 후에 안전하게 닫는 구조를 제공합니다.

## 또한 보세요 

- Clojure 공식 문서: [https://clojure.org](https://clojure.org)