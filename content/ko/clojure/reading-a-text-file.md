---
title:                "텍스트 파일 읽기"
html_title:           "Clojure: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 읽는 것은 프로그래밍에서 매우 중요한 일입니다. 프로그램에서 데이터를 읽기 위해서는 텍스트 파일을 이해하고 처리하는 방법을 알아야 합니다.

## 어떻게

```Clojure
;; 파일 읽기
(with-open [rdr (clojure.java.io/reader "example.txt")]
  (doseq [line (line-seq rdr)]
    (println line)))

"example.txt" 파일의 내용을 한 줄씩 읽어서 출력합니다.
```

```Clojure
;; 파일에 쓰기
(with-open [wrt (clojure.java.io/writer "example.txt")]
  (.write wrt "Clojure is awesome!"))

"example.txt" 파일에 "Clojure is awesome!"를 씁니다.
```

## 심층 탐구

텍스트 파일을 읽고 쓰는 것은 프로그래밍에서 매우 중요한 부분입니다. Clojure는 `clojure.java.io` 라이브러리를 제공하여 파일을 다루는데 유용한 함수를 제공합니다. `with-open` 함수는 파일을 자동으로 닫아주기 때문에 파일 처리 후 꼭 닫아줘야 하는 번거로움을 줄여줍니다. `line-seq` 함수는 파일의 내용을 한 줄씩 순회하며 가져올 수 있게 해줍니다.

## 더 알아보기

Clojure에서 파일을 읽고 쓰는 방법에 대해 더 자세히 알아보려면 아래 링크들을 참고해보세요.

* [Official Clojure Documentation on I/O](https://clojure.org/reference/io)
* [Clojure Cookbook: File I/O](https://clojure-cookbook.com/010_file_io.html)
* [Clojure for the Brave and True: Working with Files](https://www.braveclojure.com/files/)