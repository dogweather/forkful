---
title:                "명령줄 인수 읽기"
aliases:
- /ko/clojure/reading-command-line-arguments/
date:                  2024-01-20T17:55:46.510218-07:00
model:                 gpt-4-1106-preview
simple_title:         "명령줄 인수 읽기"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜?)
커맨드 라인 인자 읽기란 프로그램 실행 시 입력한 텍스트를 처리하는 것입니다. 프로그래머들이 이를 사용해 프로그램 설정을 유연하게 조정하고 다양한 시나리오에 맞게 실행하기 위함입니다.

## How to:
Clojure에서 커맨드 라인 인자를 읽는 것은 단순합니다. `*command-line-args*` 라는 전역 변수에 접근하여 값을 불러옵니다.

```clojure
;; main.clj

(defn -main
  [& args]  ; 이 부분이 커맨드 라인 인자들을 받는다.
  (println "Hello, your arguments are:" args))

;; 터미널에서 실행:
;; clojure -M main.clj arg1 arg2 arg3

;; 출력될 것:
;; Hello, your arguments are: (arg1 arg2 arg3)
```

## Deep Dive (깊은 탐색)
커맨드 라인 인자를 읽는 것은 UNIX 시스템에서 시작되었으며, 다양한 프로그래밍 언어에서 구현되었습니다. Clojure에서는 `-main` 함수와 `*command-line-args*` 전역변수를 통해 인자를 취급합니다.

대안으로, 구체적인 파싱 도구를 사용할 수 있습니다. 예를 들어, `clojure.tools.cli` 라이브러리는 보다 복잡한 인자 처리 기능을 제공합니다. 인자의 구문을 주석해주고 옵션의 유효성 검사까지 가능하게 해줍니다.

구현을 살펴보면, Clojure는 자바 가상 머신(JVM) 위에서 동작하기 때문에, 내부적으로는 자바의 인자 처리 메커니즘을 사용합니다. `*command-line-args*`는 단지 Clojure에서 쉽게 접근할 수 있도록 해주는 "wrapper"라고 볼 수 있습니다.

## See Also (참고자료)
- clojure.tools.cli GitHub: [https://github.com/clojure/tools.cli](https://github.com/clojure/tools.cli)
- Practical Clojure (실용 Clojure) - 이 책에서 다양한 Clojure 프로그래밍 패턴을 배울 수 있습니다.
