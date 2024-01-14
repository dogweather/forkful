---
title:                "Clojure: 텍스트 파일 읽기"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 읽는 것에 대해 관심을 가지는 이유는 여러 가지가 있을 수 있습니다. 예를 들어, 프로그래밍을 배우고 연습하기 위해서, 다른 프로그래머들의 코드를 읽고 이해하기 위해서, 또는 자신이 작성한 코드의 작동 여부를 확인하기 위해서 등이 있을 수 있습니다. 어떤 이유든지 간에, 텍스트 파일을 읽는 것은 프로그래밍에서 기본적이고 필요한 기술입니다.

## 어떻게

텍스트 파일을 읽는 것은 간단한 작업처럼 보일 수 있지만, Clojure에서는 여러 가지 방법으로 할 수 있습니다. 가장 기본적인 방법은 `slurp` 함수를 사용하는 것입니다. 이 함수는 파일의 전체 내용을 문자열로 반환해줍니다. 예를 들어, `slurp` 함수를 사용하여 `"sample.txt"` 파일을 읽으면 다음과 같은 결과가 나옵니다:

```Clojure
(slurp "sample.txt")
```

```
안녕하세요? 이것은 텍스트 파일의 내용입니다.
```

하지만 크기가 큰 파일을 읽는 경우에는 메모리 부족 오류가 발생할 수 있습니다. 이럴 때는 `line-seq` 함수를 사용하여 한 줄씩 파일을 읽어올 수 있습니다. 예를 들어, `line-seq` 함수를 사용하여 `"numbers.txt"` 파일을 읽어오면 다음과 같은 결과가 나옵니다:

```Clojure
(loop [lines (line-seq (clojure.java.io/reader "numbers.txt"))]
  (when-let [line (first lines)]
    (println line)
    (recur (rest lines))))
```

```
1
2
3
4
5
```

## 심층 분석

텍스트 파일을 읽는 것에 대해 더욱 깊이 들어가보면, Clojure에서는 `clojure.java.io` 라이브러리를 사용하여 파일을 읽고 쓸 수 있습니다. 이 라이브러리는 Java의 `java.io` 패키지를 Clojure에서 사용할 수 있게 해주는 매우 유용한 도구입니다. 또한, `clojure.string` 라이브러리를 사용하면 문자열을 다루는 데에 있어서 더욱 편리하게 사용할 수 있습니다.

## 참고 자료

- [Clojure 공식 문서 - 파일](https://clojure.org/reference/java_interop#_file)
- [불편한 클로져 튜토리얼 - 파일 핸들링](https://uncomfortableprogrammer.com/clojure-tutorial/#파일-핸들링)