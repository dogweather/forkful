---
title:                "임시 파일 생성하기"
html_title:           "Python: 임시 파일 생성하기"
simple_title:         "임시 파일 생성하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 무엇이고 왜요?

임시 파일 생성은 특정 절차를 거친 후 삭제되는 일시적인 파일을 생성하는 것입니다. 해야 할 일이 크고 복잡하거나 많은 양의 데이터를 임시로 저장해야 할 때 프로그래머들이 이를 사용합니다.

## 방법:

Clojure에서 temp file을 만들어봅시다.

```Clojure
(require '[clojure.java.io :as io])
(let [tf (io/make-temp-file "prefix" ".ext")]
  (println "Temp file:" (.getPath tf)))
```

이 코드는 임시 파일을 만들고, 그 경로를 출력합니다.

## 딥 다이브

임시 파일은 컴퓨팅 역사와 함께 오랜 경험을 가지고 있으며, 주로 중간 계산 결과를 저장하거나 대량의 데이터를 분석하는 등의 일시적인 용도로 사용됩니다. Clojure에서의 대안으로는 메모리나 데이터베이스에 데이터를 임시로 저장하는 방법이 있습니다. Clojure에서 io/make-temp-file 함수는 java.io.File.createTempFile 메소드를 사용하여 임시 파일을 생성하며, 이 파일은 JVM이 종료되거나 코드가 명시적으로 삭제하거나 재부팅할 때까지 존재합니다.

## 참고자료

다음은 관련 링크입니다:

- Clojure java.io API: https://clojuredocs.org/clojure.java.io
- Clojure Cookbook: https://www.clojure-cookbook.com/
- Java Temp File: https://docs.oracle.com/javase/7/docs/api/java/io/File.html#createTempFile(java.lang.String,%20java.lang.String)