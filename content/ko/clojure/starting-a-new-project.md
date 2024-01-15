---
title:                "새 프로젝트 시작하기"
html_title:           "Clojure: 새 프로젝트 시작하기"
simple_title:         "새 프로젝트 시작하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 왜?

Clojure는 다른 프로그래밍 언어와 달리, 단순한 코드와 강력한 기능을 결합하여 효율적이고 성능이 우수한 프로그램을 만들 수 있도록 해줍니다. 새로운 프로젝트를 시작하면서 Clojure를 사용하면, 보다 견고하고 간결한 코드를 작성할 수 있으며, 훌륭한 성능을 발휘할 수 있습니다.

## 시작하는 법

### 새 프로젝트 만들기

우선, Clojure를 사용할 수 있는 개발 환경을 설정해야 합니다. 이를 위해 다음과 같은 단계를 따르세요:

1. 먼저, Java 개발 키트(JDK)를 다운로드하고 설치합니다. Clojure는 Java 가상 머신 위에서 실행되므로, JDK가 필요합니다.
2. 다음으로, 빌드 관리 도구인 Leiningen을 설치합니다. Leiningen은 Clojure 프로젝트를 관리하는데 사용되며, 프로젝트 구조를 자동으로 생성하고 필요한 라이브러리를 다운로드합니다.
3. 이제 새로운 프로젝트를 생성할 준비가 되었습니다. 프로젝트 디렉토리를 생성하고, 다음 명령어를 실행하세요:

```
lein new <프로젝트명>
```

### 새로운 함수 정의하기

새 프로젝트를 시작했다면, Clojure의 강력한 기능 중 하나인 함수를 정의하는 방법을 알아보겠습니다. 함수는 다음과 같이 정의할 수 있습니다:

```
(defn 함수명 [매개변수]
    (함수 본문)) 
```

예를 들어, `plus-five`라는 이름의 함수를 정의하고, 매개변수로 숫자 `x`를 받아 `x`에 `5`를 더해주는 함수를 작성해보겠습니다:

```
(defn plus-five [x]
    (+ x 5)) 
```

이 함수를 호출하기 위해서는, 다음과 같이 입력하세요:

```
(plus-five 10) 
```

위의 호출은 `15`라는 결과값을 반환합니다.

## 더 깊게 알아보기

새로운 프로젝트를 시작할 때, Clojure에 명시적으로 종속되지 않고 다른 라이브러리를 사용하고 싶을 수 있습니다. 이런 경우 `project.clj`라는 파일에 직접 종속성을 추가할 수 있습니다. 예를 들어, `org.clojure/math.numeric-tower` 라이브러리를 추가하고 싶다면, 다음과 같은 코드를 `project.clj`에 추가하면 됩니다:

```
(defproject <프로젝트명> "0.1.0-SNAPSHOT"
    :description "My project"
    :dependencies [[org.clojure/clojure "1.10.0"]
                   [org.clojure/math.numeric-tower "0.0.4"]]) 
```

새로운 프로젝트를 시작할 때 이렇게 라이브러리를 직접 추가하면, 프로그래밍 과정에서 라이브러리를 자유롭게 사용할 수 있습니다.

## 봐도 좋아요

- [Clojure 공식 홈페이지](https://clojure.org/)
- [Clojure 문서](https://clojure.org/about/documentation)
- [Leiningen 문서](https://leiningen.org/)