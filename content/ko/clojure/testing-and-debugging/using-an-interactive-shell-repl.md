---
date: 2024-01-26 04:13:18.309513-07:00
description: "REPL, \uB610\uB294 \uC77D\uAE30-\uD3C9\uAC00-\uCD9C\uB825 \uB8E8\uD504\
  \uB294 \uB3D9\uC801\uC73C\uB85C Clojure \uCF54\uB4DC\uB97C \uC870\uAC01\uC870\uAC01\
  \ \uD14C\uC2A4\uD2B8\uD558\uB294 \uD504\uB85C\uADF8\uB798\uBC0D \uD658\uACBD\uC785\
  \uB2C8\uB2E4. \uCF54\uB4DC \uC81C\uC791\uC790\uB294 \uC989\uAC01\uC801\uC778 \uD53C\
  \uB4DC\uBC31, \uBC18\uBCF5\uC801 \uAC1C\uBC1C, \uADF8\uB9AC\uACE0 \uC804\uCCB4 \uD504\
  \uB85C\uC81D\uD2B8 \uD658\uACBD\uC744 \uAD6C\uCD95\uD558\uAC70\uB098 \uCEF4\uD30C\
  \uC77C\uD558\uB294 \uAC83 \uC5C6\uC774 \uBE60\uB974\uAC8C \uC2E4\uD5D8\uD558\uAE30\
  \ \uC704\uD574 \uC774\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
lastmod: '2024-03-11T00:14:28.578223-06:00'
model: gpt-4-0125-preview
summary: "REPL, \uB610\uB294 \uC77D\uAE30-\uD3C9\uAC00-\uCD9C\uB825 \uB8E8\uD504\uB294\
  \ \uB3D9\uC801\uC73C\uB85C Clojure \uCF54\uB4DC\uB97C \uC870\uAC01\uC870\uAC01 \uD14C\
  \uC2A4\uD2B8\uD558\uB294 \uD504\uB85C\uADF8\uB798\uBC0D \uD658\uACBD\uC785\uB2C8\
  \uB2E4. \uCF54\uB4DC \uC81C\uC791\uC790\uB294 \uC989\uAC01\uC801\uC778 \uD53C\uB4DC\
  \uBC31, \uBC18\uBCF5\uC801 \uAC1C\uBC1C, \uADF8\uB9AC\uACE0 \uC804\uCCB4 \uD504\uB85C\
  \uC81D\uD2B8 \uD658\uACBD\uC744 \uAD6C\uCD95\uD558\uAC70\uB098 \uCEF4\uD30C\uC77C\
  \uD558\uB294 \uAC83 \uC5C6\uC774 \uBE60\uB974\uAC8C \uC2E4\uD5D8\uD558\uAE30 \uC704\
  \uD574 \uC774\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uC778\uD130\uB799\uD2F0\uBE0C \uC178 (REPL) \uC0AC\uC6A9\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
REPL, 또는 읽기-평가-출력 루프는 동적으로 Clojure 코드를 조각조각 테스트하는 프로그래밍 환경입니다. 코드 제작자는 즉각적인 피드백, 반복적 개발, 그리고 전체 프로젝트 환경을 구축하거나 컴파일하는 것 없이 빠르게 실험하기 위해 이를 사용합니다.

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
