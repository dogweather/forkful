---
title:                "임시 파일 만들기"
html_title:           "Clojure: 임시 파일 만들기"
simple_title:         "임시 파일 만들기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜

임시 파일을 생성하는 일은 자주 발생하는 일은 아니지만, 프로그래밍의 여러 부분에서 도움이 됩니다. 일시적으로 수행해야 할 작업이나 데이터를 저장하고 처리할 때 유용합니다.

## 쉬운 사용법

임시 파일을 생성하려면 `java.nio.file.Files` 네임스페이스에서 `createTempFile` 함수를 사용합니다. 이 함수는 파일이 저장될 디렉토리의 경로와 파일의 접두사를 인수로 받습니다. 아래는 예제 코드와 그 결과입니다.

```Clojure
(require '[java.nio.file :as file])

;; 임시 파일 생성
(def temp-file (file/createTempFile "/tmp" "clojure"))

;; 파일 경로 출력
(println (.toString temp-file))

;; 파일 삭제
(file/delete temp-file)
```

```
/tmp/clojure3123539584376751016
```

## 깊이 파헤치기

임시 파일을 생성할 때 몇 가지 옵션을 고려해야 합니다. 첫 번째 인수로 넘긴 디렉토리 경로는 존재해야 하며, 그렇지 않으면 예외가 발생합니다. 또한 생성하려는 파일의 접두사는 한글자 이상의 문자열로 지정해야 하며, 접두사를 지정하지 않으면 임의의 문자열이 생성됩니다. 또한 `createTempFile` 함수는 파일을 생성하는 대신 파일 객체를 반환할 수 있는 `createTempDirectory` 함수도 있습니다.

## 관련 자료

[자바 공식 문서 - 임시 파일 생성](https://docs.oracle.com/javase/tutorial/essential/io/tmpdir.html)