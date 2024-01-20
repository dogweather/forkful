---
title:                "새 프로젝트 시작하기"
html_title:           "Arduino: 새 프로젝트 시작하기"
simple_title:         "새 프로젝트 시작하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

새로운 프로젝트를 시작하는 것은 기존 애플리케이션에서 독립적인 새로운 기능을 구현하거나 완전히 새로운 소프트웨어 제품을 개발하는 것을 의미합니다. 프로그래머들은 새로운 아이디어를 테스트하거나 특정 문제를 해결하기 위해 이를 수행합니다.

## 어떻게 하나:

Clojure 새 프로젝트를 시작하는 것은 여러 단계로 이루어집니다. 우선, Leiningen이라는 도구를 이용하여 프로젝트 디렉토리를 생성합니다:

```Clojure
lein new project-name
```

그런 다음, 생성된 프로젝트의 디렉토리로 이동합니다:

```Clojure
cd project-name
```

일단 프로젝트 디렉토리에 있으면, Clojure 소스 코드를 작성할 수 있습니다.

## 깊은 탐색:

새 프로젝트를 시작하는 개념은 프로그래밍의 역사 상 가장 오래되었고, 다양한 언어와 환경에 적용되었습니다. Clojure에서는 Leiningen이라는 도구를 사용하여 이를 처리하며, 이는 프로젝트의 빌드와 관리를 단순화합니다. 이외에도 Boot이나 Clojure-CLI라는 대안적인 도구도 있습니다. 

새 프로젝트를 생성하는 동안 Clojure는 일반적으로 다음과 같은 몇 가지 요소를 설정합니다:

- 프로젝트의 이름과 버전
- 프로젝트의 종속성
- Application의 시작점

## 참고:

아래는 Clojure 프로젝트 관리에 대한 추가적인 정보를 제공하는 몇몇 소스들입니다: 

- [Official Clojure](https://clojure.org/guides/getting_started) : Clojure 공식 가이드
- [Leiningen](https://leiningen.org/) : Leiningen 공식 문서
- [Boot](https://boot-clj.com/) : Boot 공식 문서
- [Clojure-CLI](https://clojure.org/reference/deps_and_cli) : Clojure-CLI 공식 문서