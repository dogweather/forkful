---
title:                "프로그래머를 위한 TOML 다루기"
date:                  2024-01-26T04:20:41.144861-07:00
model:                 gpt-4-0125-preview
simple_title:         "프로그래머를 위한 TOML 다루기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/working-with-toml.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
TOML을 사용한다는 것은 "Tom's Obvious, Minimal Language"의 최소한 형태로 데이터를 처리한다는 것을 의미하며, 읽기 쉬움 때문에 설정 파일에 인기가 있습니다. 프로그래머들은 사람이 읽기 친화적인 문법으로 박스 바깥에서 바로 작동하는 간단한 구성 관리를 위해 사용합니다.

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
