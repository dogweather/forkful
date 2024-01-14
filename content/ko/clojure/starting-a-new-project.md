---
title:                "Clojure: 새로운 프로젝트 시작하기"
programming_language: "Clojure"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 왜

새로운 프로젝트를 시작하는 것이 중요한 이유는 여러 가지가 있습니다. 프로그래밍 언어와 패러다임이 다양하게 존재하기 때문에 새로운 프로젝트를 시작하는 것은 새로운 스킬과 경험을 얻을 수 있는 좋은 기회입니다. 또한, 새로운 프로젝트를 시작함으로써 자신의 아이디어를 현실로 구현할 수 있습니다.

## 어떻게

새로운 Clojure 프로젝트를 시작하기 위해서는 먼저 Leiningen을 설치해야 합니다. 이 후, 다음과 같은 명령어를 사용하여 프로젝트를 생성할 수 있습니다.

```Clojure
lein new app [프로젝트 이름]
```

이렇게 생성된 프로젝트 디렉토리 안에는 기본적인 Clojure 파일들이 존재하며, 여기서부터 새로운 코드를 작성하면 됩니다.

새로운 함수를 만드는 예시는 다음과 같습니다.

```Clojure
(defn hello-world []
  (println "안녕 세상!"))
```

이 함수는 콘솔에 "안녕 세상!"을 출력하는 함수입니다. 이 함수를 실행하려면 다음과 같은 코드를 입력합니다.

```Clojure
(hello-world)
```

아래는 실행 결과입니다.

```
안녕 세상!
```

## 심층 분석

새로운 프로젝트를 시작할 때, 가장 중요한 것은 무엇을 만들고자 하는지 명확하게 정의하는 것입니다. 이를 위해 목표와 요구사항을 충분히 분석하고 문서화하는 것이 필요합니다. 또한, 다른 프로그래밍 언어와의 차이점을 파악하고 Clojure의 장점을 최대한 활용하는 것이 중요합니다. 이를 위해 Clojure 공식 문서나 다른 개발자들의 블로그 등을 참고하는 것이 도움이 될 수 있습니다.

## See Also

- [Clojure 공식 문서](https://clojure.org/index#resources)
- [모두의 코드 - Clojure 패스](https://modoocode.com/244)
- [Clojure 학교 - 기초 문법](http://www.clojureschool.com/)