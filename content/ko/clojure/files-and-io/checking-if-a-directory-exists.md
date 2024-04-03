---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:28.300526-07:00
description: "\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694: \uD074\uB85C\uC800\uB294 JVM\
  \ \uC5B8\uC5B4\uB85C, \uC774 \uBAA9\uC801\uC744 \uC704\uD574 \uC790\uBC14\uC758\
  \ `java.io.File` \uD074\uB798\uC2A4\uB97C \uC0AC\uC6A9\uD560 \uC218 \uC788\uC2B5\
  \uB2C8\uB2E4. \uC774\uB7EC\uD55C \uAE30\uBCF8\uC801\uC778 \uC791\uC5C5\uC744 \uC704\
  \uD574 \uC81C3\uC758 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uAC00 \uD544\uC694\uD558\uC9C0\
  \ \uC54A\uC2B5\uB2C8\uB2E4. \uB2E4\uC74C\uC740 \uC5B4\uB5BB\uAC8C \uD558\uB294\uC9C0\
  \uC5D0 \uB300\uD55C \uBC29\uBC95\uC785\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.682138-06:00'
model: gpt-4-0125-preview
summary: "\uD074\uB85C\uC800\uB294 JVM \uC5B8\uC5B4\uB85C, \uC774 \uBAA9\uC801\uC744\
  \ \uC704\uD574 \uC790\uBC14\uC758 `java.io.File` \uD074\uB798\uC2A4\uB97C \uC0AC\
  \uC6A9\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uB514\uB809\uD1A0\uB9AC\uAC00 \uC874\uC7AC\uD558\uB294\uC9C0 \uD655\uC778\
  \uD558\uAE30"
weight: 20
---

## 어떻게 하나요:
클로저는 JVM 언어로, 이 목적을 위해 자바의 `java.io.File` 클래스를 사용할 수 있습니다. 이러한 기본적인 작업을 위해 제3의 라이브러리가 필요하지 않습니다. 다음은 어떻게 하는지에 대한 방법입니다:

```clojure
(import 'java.io.File)

(defn directory-exists? [dir-path]
  (let [dir (File. dir-path)]
    (.exists dir)))

;; 사용 예시
(println (directory-exists? "/path/to/your/directory")) ;; true 또는 false
```

이 함수인 `directory-exists?`는 디렉토리 경로를 문자열로 받아, 디렉토리가 존재하면 `true`를, 그렇지 않으면 `false`를 반환합니다. 이는 디렉토리 경로로 `File` 객체를 생성한 다음, 이 객체에 `.exists` 메서드를 호출하여 달성됩니다.

자바 상호운용성 뿐만 아니라, 자바의 보일러플레이트를 일부 추상화하는 클로저 라이브러리를 사용할 수도 있습니다. 그중 하나가 `clojure.java.io`입니다. 하지만, 디렉토리의 존재 여부를 확인할 때는 `File` 클래스를 여전히 사용할 것이지만, 다른 파일 작업을 위해 이 라이브러리가 유용할 수 있습니다. 예시:

```clojure
(require '[clojure.java.io :as io])

(defn directory-exists?-clojure [dir-path]
  (.exists (io/file dir-path)))

;; 예제 사용
(println (directory-exists?-clojure "/another/path/to/check")) ;; true 또는 false
```

이 버전은 매우 유사하지만, `File` 객체를 생성하기 위해 클로저의 `io/file` 함수를 사용합니다. 이 메서드는 자바 클래스와 직접적으로 인터페이스하는 것보다 클로저 코드 기반에 더 자연스럽게 융합됩니다. 클로저의 IO 작업 라이브러리를 활용하여 이를 통해 이루어집니다.
