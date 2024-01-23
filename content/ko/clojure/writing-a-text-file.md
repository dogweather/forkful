---
title:                "텍스트 파일 작성하기"
html_title:           "Arduino: 텍스트 파일 작성하기"
simple_title:         "텍스트 파일 작성하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
텍스트 파일 작성은 문자 데이터를 파일로 저장하는 과정입니다. 프로그래머들은 설정, 로그, 데이터 교환 등을 위해 이를 수행합니다.

## How to: (어떻게 하나요?)
Clojure에서는 `spit` 함수를 사용하여 텍스트 파일을 쉽게 작성할 수 있습니다.

```Clojure
(spit "example.txt" "Hello, Clojure readers!")
```

만약 파일을 실행하면, `example.txt` 파일에 "Hello, Clojure readers!" 텍스트가 저장됩니다.

데이터를 추가할 경우 `spit` 함수에 `:append` 옵션을 사용하세요.

```Clojure
(spit "example.txt" "\nAdd more lines." :append true)
```

## Deep Dive (심층 분석)
Clojure의 `spit` 함수는 내부적으로 Java의 `java.io.PrintWriter`를 사용합니다. 2007년에 발표된 Clojure는 JVM 위에서 동작하는 언어로, 자바의 입출력 API를 활용합니다. `spit` 이외에도 `clojure.java.io/writer`를 사용한 더 저수준의 파일 작성 방법이 있습니다.

```Clojure
(with-open [w (clojure.java.io/writer "example.txt" :append true)]
  (.write w "\nAnother line."))
```

이 코드는 열린 파일에 문자열을 추가한 후 파일을 자동으로 닫습니다.

## See Also (함께 보기)
- [ClojureDocs on `spit`](https://clojuredocs.org/clojure.core/spit)
- [Clojure Input/Output](https://clojure.org/guides/learn/functions#_io)
- [Clojure 공식 문서](https://clojure.org/)
- [`clojure.java.io` API](https://clojuredocs.org/clojure.java.io)
