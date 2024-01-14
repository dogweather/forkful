---
title:                "Clojure: 텍스트 파일 작성하기"
simple_title:         "텍스트 파일 작성하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

# 왜

왜 누군가가 텍스트 파일을 작성하는 것에 참여하게 될까요? Clojure 프로그래밍 언어는 다양한 목적으로 사용될 수 있지만, 텍스트 파일을 작성하는 것은 중요한 부분입니다. 텍스트 파일은 다른 프로그램에서 사용되거나, 데이터 저장 용도로 사용될 수 있기 때문입니다.

# 어떻게

```Clojure
;; Clojure에서 텍스트 파일을 작성하는 방법
(with-open [file (clojure.java.io/writer "myFile.txt")]
  (.write file "안녕하세요?")
  (.write file "Clojure는 멋진 언어입니다!"))

(with-open [file (clojure.java.io/reader "myFile.txt")]
  (println (.readLine file))
  (println (.readLine file)))
```
```
출력:
안녕하세요?
Clojure는 멋진 언어입니다!
```

# 깊이 파고들기

이제 우리는 Clojure로 간단한 텍스트 파일을 작성하는 방법을 알게 되었습니다. 하지만 우리는 더 많은 것을 배울 수 있습니다. Clojure에서는 java.io 라이브러리를 사용하여 파일과 관련된 다양한 작업을 수행할 수 있습니다. 파일을 읽고 쓰는 것 뿐만 아니라, 파일의 존재 여부를 확인하고 새로운 파일을 생성할 수도 있습니다.

# 관련 자료

## Clojure 공식 문서 - 파일 다루기
https://clojuredocs.org/clojure.java.io/file

## Clojure 공식 홈페이지
https://clojure.org/

### 참고 링크
- 쓰기 예제: https://clojure.org/guides/io#writing
- 읽기 예제: https://clojure.org/guides/io#reading
- 파일 존재 여부 확인: https://clojure.org/guides/io#test-file