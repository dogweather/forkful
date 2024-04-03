---
date: 2024-01-20 18:03:29.139390-07:00
description: "\uC2E4\uD589 \uBC29\uBC95: Clojure \uD504\uB85C\uC81D\uD2B8\uB97C \uC2DC\
  \uC791\uD558\uB824\uBA74 Leiningen\uC774\uB77C\uB294 \uBE4C\uB4DC \uB3C4\uAD6C\uB97C\
  \ \uC0AC\uC6A9\uD558\uB294 \uAC83\uC774 \uC77C\uBC18\uC801\uC785\uB2C8\uB2E4. \uC774\
  \ \uC608\uC81C\uB294 \uAC04\uB2E8\uD55C Clojure \uC560\uD50C\uB9AC\uCF00\uC774\uC158\
  \uC744 \uC0DD\uC131\uD558\uB294 \uBC29\uBC95\uC744 \uBCF4\uC5EC\uC90D\uB2C8\uB2E4\
  ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:54.661390-06:00'
model: gpt-4-1106-preview
summary: "Clojure \uD504\uB85C\uC81D\uD2B8\uB97C \uC2DC\uC791\uD558\uB824\uBA74 Leiningen\uC774\
  \uB77C\uB294 \uBE4C\uB4DC \uB3C4\uAD6C\uB97C \uC0AC\uC6A9\uD558\uB294 \uAC83\uC774\
  \ \uC77C\uBC18\uC801\uC785\uB2C8\uB2E4."
title: "\uC0C8 \uD504\uB85C\uC81D\uD2B8 \uC2DC\uC791\uD558\uAE30"
weight: 1
---

## 실행 방법:
Clojure 프로젝트를 시작하려면 Leiningen이라는 빌드 도구를 사용하는 것이 일반적입니다. 이 예제는 간단한 Clojure 애플리케이션을 생성하는 방법을 보여줍니다.

```Clojure
; Leiningen을 설치합니다.
; 터미널에서 새 프로젝트를 만드는 명령어를 입력합니다.
lein new app my-cool-app

; 생성된 프로젝트 폴더로 이동합니다.
cd my-cool-app

; 주요 애플리케이션 코드는 src/my_cool_app/core.clj 파일에 있습니다.
; core.clj 파일을 열고
(ns my-cool-app.core
  (:gen-class))

(defn -main
  [& args]
  (println "안녕, 여러분! 이것이 내 첫 Clojure 프로젝트입니다!"))
  
; 프로젝트를 실행합니다.
lein run

; 출력 예시:
; 안녕, 여러분! 이것이 내 첫 Clojure 프로젝트입니다!
```

## 심층 분석:
Clojure는 2007년 Rich Hickey에 의해 고안되었고, 불변성과 함수형 프로그래밍이 주된 특징입니다. 프로젝트를 실행하고 관리하기 위해 Leiningen, Boot, Clojure CLI 등의 도구를 사용할 수 있습니다. Leiningen은 가장 인기 있는 도구로, 프로젝트 구성, 종속성 관리, 빌드 자동화 등을 쉽게 만들어주죠. `lein new app` 명령은 기본 프로젝트 구조를 생성하며, 여기에는 소스 코드, 테스트 케이스, 리소스 디렉터리, 그리고 프로젝트 설정을 위한 project.clj 파일 등이 포함됩니다.

## 관련 자료:
- Leiningen 공식 웹사이트: [https://leiningen.org/](https://leiningen.org/)
- Clojure 공식 문서: [https://clojure.org/guides/getting_started](https://clojure.org/guides/getting_started)
- Clojure 동사협회: [http://clojure.kr/](http://clojure.kr/)
