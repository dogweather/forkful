---
title:                "디렉토리가 존재하는지 확인하기"
html_title:           "Bash: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 무엇이며 왜?

디렉터리 존재 확인이란 소프트웨어가 특정 디렉터리의 존재 여부를 파악하는 등의 작업을 말합니다. 이러한 작업은 파일의 있음 없음에 따라 동작을 수행하거나 오류를 방지하는 등, 다양한 코딩 시나리오에서 요구됩니다.

## 어떻게:

Clojure에서는 'clojure.java.io' 라이브러리의 'file' 함수와 Java의 'exists' 메서드를 사용하여 디렉터리 존재 확인을 합니다.

```Clojure
(require '[clojure.java.io :as io])

(defn directory-exists? [path]
  (.exists (io/file path)))
```

이 함수를 사용하여 디렉터리의 존재 여부를 확인 가능합니다.

```Clojure
(directory-exists? "/path/to/directory")
```

## 딥 다이브:

디렉터리 존재 확인은 컴퓨터 프로그래밍의 초기부터 필요한 작업이었습니다. 요즘에는 대부분의 언어가 이 기능을 내장하고 있습니다. Clojure도 마찬가지이며 Java의 API를 활용하여 이 작업을 처리합니다. 

대안적으로, 운영 체제의 명령어(shell commands)를 사용할 수도 있지만, 이는 플랫폼 종속적인 측면이 있으며, 순수 Clojure 코드를 사용하는 것보다 에러 처리도 복잡합니다.

'io/file' 함수와 '.exists' 메서드의 사용은 매우 간단합니다. 'io/file' 함수는 제공된 경로로 파일 객체를 생성하고, '.exists' 메서드는 해당 파일(또는 디렉터리)가 실제로 존재하는지 확인합니다.

## 참고 자료:

1. Clojure 공식 문서: [clojure.java.io](https://clojure.github.io/clojure/clojure.java.io-api.html#clojure.java.io/file)
2. Java 공식 문서: [File](https://docs.oracle.com/javase/7/docs/api/java/io/File.html#exists())