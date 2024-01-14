---
title:                "Clojure: yaml로 작업하기"
simple_title:         "yaml로 작업하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

## 왜 YAML과 함께 작업하는가?

YAML은 인간이 읽고 쓰기 쉽고 기계가 이해하기 쉬운 형식으로 데이터를 저장하는 데 사용되는 인기있는 형식입니다. Clojure 프로그래밍 언어를 사용하면 YAML 데이터를 손쉽게 처리하고 조작할 수 있습니다.

## 어떻게 YAML을 다룰 수 있는가?

Clojure에서 YAML을 다루는 방법은 간단합니다. 먼저, `de.dante.yaml` 라이브러리를 프로젝트에 추가해줍니다. 그런 다음, `yaml/parse` 함수를 사용하여 YAML 파일에서 데이터를 추출합니다. 예를 들어, `test.yaml` 파일이 다음과 같은 데이터를 포함하고 있다고 가정해 봅시다.

```Clojure
{:name "John Doe"
 :age 25
 :hobbies ["hiking" "reading" "cooking"]
 :address {:street "123 Main St"
           :city "New York"
           :state "NY"}}
```

다음 코드를 사용하여 해당 파일을 파싱하고 데이터를 추출할 수 있습니다.

```Clojure
(ns my-project.core
  (:require [de.dante.yaml :as yaml]))

(def data (yaml/parse "test.yaml"))
```

이제 `data` 변수에는 YAML 파일에서 추출한 데이터가 포함되어 있습니다. 예를 들어, `(:name data)`를 실행하면 "John Doe" 스트링이 반환됩니다. `(:hobbies data)`를 실행하면 "hiking", "reading", "cooking"이 포함되어 있는 벡터가 반환됩니다.

## YAML 깊이 알아보기

YAML은 쉽게 읽고 쓸 수 있지만, 조금 더 심층적으로 살펴보면 몇 가지 유용한 기능을 발견할 수 있습니다. `de.dante.yaml` 라이브러리는 Clojure 데이터 구조를 이용하여 YAML 파일을 읽고 쓸 수 있도록 해 줍니다. 이를 통해 우리는 YAML 파일을 조작하고 필요한 데이터를 추출할 수 있습니다.

또한 YAML은 다른 포맷과의 변환이 쉽습니다. 예를 들어, `yaml/generate` 함수를 사용하면 Clojure 데이터 구조를 YAML 형식으로 변환할 수 있습니다. 이를 통해 우리는 Clojure에서 YAML로 데이터를 작성할 수 있습니다.

## 관련 링크

- [de.dante.yaml 라이브러리](https://github.com/dakrone/clj-yaml)
- [Clojure 공식 홈페이지](https://clojure.org/)
- [YAML 공식 홈페이지](https://yaml.org/)