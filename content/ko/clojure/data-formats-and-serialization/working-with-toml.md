---
date: 2024-01-26 04:20:41.144861-07:00
description: "\uBC29\uBC95: Clojure\uC5D0\uC11C TOML\uC744 \uC0AC\uC6A9\uD558\uAE30\
  \ \uC704\uD574\uC11C\uB294 `clj-toml`\uACFC \uAC19\uC740 \uB77C\uC774\uBE0C\uB7EC\
  \uB9AC\uAC00 \uD544\uC694\uD569\uB2C8\uB2E4. \uBA3C\uC800, \uC774\uB97C `deps.edn`\uC5D0\
  \ \uCD94\uAC00\uD558\uC138\uC694."
lastmod: '2024-03-13T22:44:54.695520-06:00'
model: gpt-4-0125-preview
summary: "Clojure\uC5D0\uC11C TOML\uC744 \uC0AC\uC6A9\uD558\uAE30 \uC704\uD574\uC11C\
  \uB294 `clj-toml`\uACFC \uAC19\uC740 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uAC00 \uD544\
  \uC694\uD569\uB2C8\uB2E4."
title: "\uD504\uB85C\uADF8\uB798\uBA38\uB97C \uC704\uD55C TOML \uB2E4\uB8E8\uAE30"
weight: 39
---

## 방법:
Clojure에서 TOML을 사용하기 위해서는 `clj-toml`과 같은 라이브러리가 필요합니다. 먼저, 이를 `deps.edn`에 추가하세요:

```clojure
{:deps {clj-toml {:mvn/version "0.5.0"}}}
```

그 다음, 몇 가지 TOML을 분석합니다:

```clojure
(require '[clj-toml.core :as toml])

(def config-str "title = 'TOML Example'")

(def parsed-config (toml/parse-string config-str))

;; 분석된 TOML에서 제목 가져오기
(println (:title parsed-config)) ;; 출력: TOML Example
```

TOML 생성:

```clojure
(def data {:title "TOML Example"})

(println (toml/generate-string data))
;; 출력: title = "TOML Example"
```

## 심층 탐구
TOML은 설정 파일을 위한 YAML 및 JSON에 대한 간단한 대안으로, 2013년경 GitHub의 공동 창립자인 Tom Preston-Werner에 의해 만들어졌습니다. 이는 명확성을 목표로 하며 추가 도구 없이 인간이 읽을 수 있는 사양이 되려고 합니다.

JSON은 종종 API와 웹 앱용으로 사용되며, YAML은 참조와 스크립트 기능으로 복잡해질 수 있지만, TOML은 간단한 테이블 기반 구조에 중점을 두어 돋보입니다. 이러한 단순성 덕분에 특히 Rust 커뮤니티와 다른 현대 언어 환경에서 인기가 있습니다.

단순성과 실용성에 중점을 둔 Clojure는 설정에 있어 TOML과 잘 어울립니다. `clj-toml` 또는 대체 라이브러리는 격차를 메우며 TOML의 정적 데이터를 Clojure의 동적, 함수형 세계로 번역합니다.

## 참고
- TOML의 GitHub 저장소: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- `clj-toml` on Clojars: [clojars.org/clj-toml](https://clojars.org/clj-toml)
- Clojure 문서: [clojure.org](https://clojure.org/guides/getting_started)
- `clj-toml` 소개: [github.com/lantiga/clj-toml](https://github.com/lantiga/clj-toml)
