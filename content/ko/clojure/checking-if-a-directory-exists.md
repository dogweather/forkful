---
title:                "디렉토리가 존재하는지 확인하기"
html_title:           "Clojure: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Clojure로 디렉토리 존재 여부 확인하기

## What & Why?

디렉토리 존재 여부를 확인하는 것은 컴퓨터 프로그래밍에서 꽤 일반적인 작업입니다. 프로그래머들은 디렉토리가 있는지를 확인하는 이유는 다양합니다. 몇 가지 예를 들어보겠습니다. 예를 들어, 프로그래머는 특정 디렉토리에 파일이나 데이터가 존재하는지를 확인할 수 있습니다. 또는 프로그램 실행 중에 특정 파일 또는 디텍터리가 있는지에 따라 다른 작업을 수행할 수 있습니다.

## How to:

디렉토리 존재 여부를 확인하는 방법은 매우 간단합니다. Clojure에서는 `clojure.java.io/file` 함수를 사용하여 디렉토리 경로를 전달하면 해당 디렉토리가 존재하는지를 확인할 수 있습니다.

```Clojure 
(clojure.java.io/file "/Users/user/Downloads/")
```
위 코드를 실행하면, 다음과 같이 디렉토리 존재 여부를 확인하는 정보를 출력할 수 있습니다.

```Clojure
#object[java.io.File 0x10d36a32 "/Users/user/Downloads/"]
```

또 다른 방법으로는 `clojure.java.io/file-exists?` 함수를 사용할 수 있습니다. 이 함수는 디렉토리 경로를 전달받아 해당 디렉토리가 존재하는지를 불리언 값으로 반환합니다.

```Clojure
(clojure.java.io/file-exists? "/Users/user/Downloads/")
```

위 코드를 실행하면, 디렉토리가 존재하면 `true`를, 그렇지 않다면 `false`를 반환합니다.

## Deep Dive:

디렉토리 존재 여부를 확인하는 작업은 주로 파일 시스템 또는 운영체제의 기능인데, Clojure의 `clojure.java.io` 이외에도 `clojure.java.shell` 네임스페이스를 사용하여 해당 기능을 수행할 수 있습니다. `clojure.java.shell`은 외부 명령어를 실행하고 그 결과를 캡처하는 기능을 제공합니다. 이를 통해 디렉토리 존재 여부를 확인하는 작업도 수행할 수 있습니다.

또한, 디렉토리 존재 여부를 확인하는 다른 방법으로는 `java.nio.Files` 클래스의 `exist()` 메소드를 이용하는 방법이 있습니다. 이 방법은 Clojure 코드 내에서 자바의 기본 라이브러리를 사용하여 디렉토리를 확인하는 효율적인 방법입니다.

## See Also:

- [Clojure 공식 문서](https://clojure.org/index)
- [Clojure 공식 가이드](https://clojure.org/guides/getting_started)