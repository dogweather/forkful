---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:28.300526-07:00
description: "\uD074\uB85C\uC800\uC5D0\uC11C \uB514\uB809\uD1A0\uB9AC\uC758 \uC874\
  \uC7AC \uC5EC\uBD80\uB97C \uD655\uC778\uD558\uB294 \uAC83\uC740 \uD074\uB85C\uC800\
  \ \uC560\uD50C\uB9AC\uCF00\uC774\uC158 \uB0B4\uC5D0\uC11C \uD30C\uC77C \uC2DC\uC2A4\
  \uD15C \uB514\uB809\uD1A0\uB9AC\uC758 \uC874\uC7AC\uB97C \uAC80\uC99D\uD558\uB294\
  \ \uC791\uC5C5\uC785\uB2C8\uB2E4. \uC774 \uC791\uC5C5\uC740 \uD30C\uC77C \uC791\uC5C5\
  \uC5D0 \uC788\uC5B4 \uC911\uC694\uD558\uBA70, \uC874\uC7AC\uD558\uC9C0 \uC54A\uC744\
  \ \uC218 \uC788\uB294 \uB514\uB809\uD1A0\uB9AC\uC5D0\uC11C \uC77D\uAC70\uB098 \uC4F0\
  \uB294 \uB3D9\uC548 \uC624\uB958\uB97C \uBC29\uC9C0\uD558\uC5EC, \uB85C\uBC84\uC2A4\
  \uD2B8\uD558\uACE0 \uC624\uB958 \uC5C6\uB294 \uCF54\uB4DC \uC2E4\uD589\uC744\u2026"
lastmod: 2024-02-19 22:05:13.624680
model: gpt-4-0125-preview
summary: "\uD074\uB85C\uC800\uC5D0\uC11C \uB514\uB809\uD1A0\uB9AC\uC758 \uC874\uC7AC\
  \ \uC5EC\uBD80\uB97C \uD655\uC778\uD558\uB294 \uAC83\uC740 \uD074\uB85C\uC800 \uC560\
  \uD50C\uB9AC\uCF00\uC774\uC158 \uB0B4\uC5D0\uC11C \uD30C\uC77C \uC2DC\uC2A4\uD15C\
  \ \uB514\uB809\uD1A0\uB9AC\uC758 \uC874\uC7AC\uB97C \uAC80\uC99D\uD558\uB294 \uC791\
  \uC5C5\uC785\uB2C8\uB2E4. \uC774 \uC791\uC5C5\uC740 \uD30C\uC77C \uC791\uC5C5\uC5D0\
  \ \uC788\uC5B4 \uC911\uC694\uD558\uBA70, \uC874\uC7AC\uD558\uC9C0 \uC54A\uC744 \uC218\
  \ \uC788\uB294 \uB514\uB809\uD1A0\uB9AC\uC5D0\uC11C \uC77D\uAC70\uB098 \uC4F0\uB294\
  \ \uB3D9\uC548 \uC624\uB958\uB97C \uBC29\uC9C0\uD558\uC5EC, \uB85C\uBC84\uC2A4\uD2B8\
  \uD558\uACE0 \uC624\uB958 \uC5C6\uB294 \uCF54\uB4DC \uC2E4\uD589\uC744\u2026"
title: "\uB514\uB809\uD1A0\uB9AC\uAC00 \uC874\uC7AC\uD558\uB294\uC9C0 \uD655\uC778\
  \uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇을 하고 왜 하나요?
클로저에서 디렉토리의 존재 여부를 확인하는 것은 클로저 애플리케이션 내에서 파일 시스템 디렉토리의 존재를 검증하는 작업입니다. 이 작업은 파일 작업에 있어 중요하며, 존재하지 않을 수 있는 디렉토리에서 읽거나 쓰는 동안 오류를 방지하여, 로버스트하고 오류 없는 코드 실행을 보장합니다.

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
