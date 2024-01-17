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

오늘은 잠시 동안만 필요한 파일을 생성하는 작업을 배워봅시다. 개발자들은 왜 임시 파일을 만들까요? 일단 무엇인지 알아봐야겠죠.

## 무슨 일인가?

잠시 동안 필요한 파일, 즉 임시 파일은 일시적으로 사용되지만 필요한 작업입니다. 이것은 메모리에 저장된 데이터를 디스크에 저장하는 데 사용됩니다.

## 어떻게 해야 할까요?

Clojure에서 임시 파일을 만드는 방법은 간단합니다. 이를 위해서는 `java.io` 라이브러리를 사용합니다. 아래 예제를 참고해보세요:

```Clojure
(ns example.core
  (:import java.io.File))

;; 임시 파일을 생성합니다.
(with-open [temp-file (File/createTempFile "example" ".txt")]
  (println "임시 파일 경로:" (.getAbsolutePath temp-file)))
```

위 코드를 실행하면 다음과 같은 출력이 나옵니다:

```
임시 파일 경로: /var/folders/pm/_q8vs4v95vz0jv9cnsj1zgsc0000gn/T/example7650814321514391.txt
```

`File/createTempFile` 함수는 두 개의 인수를 가집니다. 첫 번째 인자는 접두사(prefix)이고 두 번째 인자는 확장자(suffix)입니다. 위 예제에서는 "example.txt"라는 이름의 임시 파일을 생성하도록 지정했습니다.

## 깊이 들어가보기

이제 위에서 보았던 예제 코드를 좀 더 자세히 살펴보겠습니다.

`File/createTempFile` 함수는 Java의 `java.io.File` 클래스를 사용합니다. 이 함수는 자바 런타임 환경에서 제공되는 기능을 활용하기 때문에 임시 파일을 생성하는 데에 안정성을 보장합니다.

만약 Clojure 외의 언어를 사용한다면 어떻게 임시 파일을 생성할 수 있을까요? 대부분의 언어에서는 임시 파일 생성을 위해 운영체제에서 제공하는 기능을 사용합니다. 만약 Java를 포함한 다른 언어를 사용한다면, 해당 언어에서 제공하는 관련 기능을 찾아보세요.

Clojure에서 임시 파일을 생성하는 방법 뿐만 아니라, 또 다른 방법으로는 `tempfile` 라이브러리를 사용하는 것도 있습니다. 이 라이브러리는 OS에서 제공하는 기능을 사용하지 않고, Clojure 코드로 직접 임시 파일을 생성할 수 있도록 도와줍니다.

## 더 알아보기

임시 파일 생성에 대해 더 자세히 알고 싶다면 아래 링크를 확인해보세요:

- [Java의 `java.io.File` 클래스 문서](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [`tempfile` 라이브러리 문서](https://github.com/posbourne/tempfile)