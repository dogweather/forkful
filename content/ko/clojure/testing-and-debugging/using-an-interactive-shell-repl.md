---
date: 2024-01-26 04:13:18.309513-07:00
description: "\uBC29\uBC95: REPL\uC740 Lisp \uACC4\uC5F4\uC758 \uC0C1\uD638\uC791\uC6A9\
  \uC801 \uAC1C\uBC1C \uCCA0\uD559\uC758 \uD575\uC2EC\uC774\uBA70, \uD604\uB300 Lisp\
  \ \uBC29\uC5B8\uC778 Clojure\uB294 \uC774 \uB3C4\uAD6C\uB97C \uD06C\uAC8C \uD65C\
  \uC6A9\uD569\uB2C8\uB2E4. \uC774\uB294 1950\uB144\uB300 \uD6C4\uBC18\uC5D0 \uCC98\
  \uC74C\uC73C\uB85C \uC18C\uAC1C\uB41C Lisp REPL\uB85C \uAC70\uC2AC\uB7EC \uC62C\uB77C\
  \uAC11\uB2C8\uB2E4. \uB2E4\uB978 \uC5B8\uC5B4\uC5D0\uC11C\uC758 \uB300\uC548\uC73C\
  \uB85C\uB294 Python\uC758 \uC778\uD130\uD504\uB9AC\uD130\uC640\u2026"
lastmod: '2024-04-05T21:53:56.506297-06:00'
model: gpt-4-0125-preview
summary: "REPL\uC740 Lisp \uACC4\uC5F4\uC758 \uC0C1\uD638\uC791\uC6A9\uC801 \uAC1C\
  \uBC1C \uCCA0\uD559\uC758 \uD575\uC2EC\uC774\uBA70, \uD604\uB300 Lisp \uBC29\uC5B8\
  \uC778 Clojure\uB294 \uC774 \uB3C4\uAD6C\uB97C \uD06C\uAC8C \uD65C\uC6A9\uD569\uB2C8\
  \uB2E4."
title: "\uC778\uD130\uB799\uD2F0\uBE0C \uC178 (REPL) \uC0AC\uC6A9\uD558\uAE30"
weight: 34
---

## 방법:
REPL 시작하기:

```Clojure
user=> (println "Hello, REPL!")
Hello, REPL!
nil
```

함수 정의하고 시도해보기:
```Clojure
user=> (defn greet [name] (str "Hello, " name "!"))
#'user/greet
user=> (greet "Clojure 프로그래머")
"Hello, Clojure 프로그래머!"
```

데이터 구조체 실험하기:
```Clojure
user=> (def my-map {:a 1 :b 2})
#'user/my-map
user=> (assoc my-map :c 3)
{:a 1, :b 2, :c 3}
```

## 심층 탐구
REPL은 Lisp 계열의 상호작용적 개발 철학의 핵심이며, 현대 Lisp 방언인 Clojure는 이 도구를 크게 활용합니다. 이는 1950년대 후반에 처음으로 소개된 Lisp REPL로 거슬러 올라갑니다. 다른 언어에서의 대안으로는 Python의 인터프리터와 Node.js의 콘솔이 있지만, Clojure의 REPL은 일급 시민으로서 작업 흐름에 필수적입니다.

Clojure REPL 세션은 커맨드라인, IDE(예: IntelliJ with Cursive 또는 Emacs with CIDER), 그리고 Nightcode와 같은 브라우저 기반 도구와 같은 다양한 환경에 통합될 수 있습니다. 더 깊은 의미에서, REPL은 개발자가 런타임에 언어의 구조를 조작하고 다양한 변환을 거쳐 상태를 유지하게 함으로써 탐색적 프로그래밍을 가능하게 하고 더 견고한 코드를 만들 수 있게 합니다.

`lein repl`이나 `clj`와 같은 도구를 사용하는 REPL의 기능성은 의존성 관리, 다양한 플러그인, 그리고 프로젝트별 맞춤형 설정을 허용하여 더 생산적이고 유연한 개발 프로세스로 이끕니다.

## 참고 자료
- REPL에 대한 공식 Clojure 웹사이트 가이드: https://clojure.org/guides/repl/introduction
- Rich Hickey의 REPL 기반 개발에 대한 강연: https://www.youtube.com/watch?v=Qx0-pViyIDU
- Practical Clojure: 반복적 개발을 위한 REPL 사용하기: http://practicalclj.blogspot.com/2009/10/using-clojure-repl.html
