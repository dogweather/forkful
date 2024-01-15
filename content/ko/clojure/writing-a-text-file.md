---
title:                "텍스트 파일 작성하기"
html_title:           "Clojure: 텍스트 파일 작성하기"
simple_title:         "텍스트 파일 작성하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 써야만 할까요? 텍스트 파일은 데이터를 쉽게 저장하고 공유하는 데 유용합니다. 또한 텍스트 파일은 사람이 읽고 쓸 수 있는 형식이기 때문에 다른 프로그래밍 언어에서도 쉽게 읽을 수 있습니다.

## 사용 방법

```Clojure
(defn write-file [file-name]
  (with-open [wrtr (clojure.java.io/writer file-name :append true)]
    (.write wrtr "Hello World!")
    (.close wrtr)))

(write-file "test.txt")
```

위의 코드는 "Hello World!" 라는 내용을 가지는 "test.txt" 파일을 생성합니다. 파일을 쓰기 위해서는 `with-open` 함수를 사용하고, `clojure.java.io/writer` 함수로 파일을 열어서 `.write` 함수를 통해 내용을 씁니다. 마지막으로 `.close` 함수를 이용해 파일을 닫습니다.

## 깊은 곳으로

텍스트 파일은 매우 유용한 데이터 저장 방식 중 하나입니다. 텍스트 파일에는 다양한 데이터를 쉽게 저장할 수 있고, 특히 CSV 형식으로 저장할 경우 데이터베이스의 대안으로 사용할 수 있습니다. 또한 텍스트 파일은 버전 관리 시스템에서도 매우 유용하며, 프로그램에서 읽고 쓰기가 쉬운 형식이기 때문에 다른 프로그램과의 데이터 교환에도 매우 유용합니다.

## 더 많은 정보

https://clojure.org/ - 공식 Clojure 웹사이트

https://www.clojure.or.kr/ - 한국 채택센터의 Clojure 정보 사이트

https://github.com/clojure/clojure - Clojure GitHub 저장소