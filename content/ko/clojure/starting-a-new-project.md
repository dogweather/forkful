---
title:                "새로운 프로젝트 시작하기"
html_title:           "Clojure: 새로운 프로젝트 시작하기"
simple_title:         "새로운 프로젝트 시작하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

새 프로젝트를 시작한다는 것은 단순히 새로운 코드를 작성하는 것을 의미합니다. 프로그래머들은 자신의 아이디어를 현실화하기 위해 새 프로젝트를 시작합니다. 또 다른 이유는 기존의 코드보다 더 나은 코드를 만들기 위해서입니다.

## 어떻게:

### 새 프로젝트 만들기:
```Clojure 
(defproject my-project "0.1.0-SNAPSHOT"
  :description "My new project"
  :dependencies [[org.clojure/clojure "1.10.1"]]
  :main my-project.core)
```

### 새 파일 만들기:
```Clojure
(defn hello-world [name]
  (println (str "Hello, " name "!")))
```

### 함수 호출하기:
```Clojure
(hello-world "John")
```

Output:
```
Hello, John !
```

## 더 깊게 들어가보기:

### 역사적 배경:
Clojure는 Rich Hickey가 2007년에 발표한 함수형 프로그래밍 언어입니다. Clojure는 JVM에서 실행되는 리스프 변종으로서 Java와의 상호 운용성을 강조하며 개발되었습니다.

### 대안:
Clojure 외에도 새 프로젝트를 시작하는 다른 방법들이 있습니다. Java나 JavaScript와 같은 다른 언어들을 사용하여 프로젝트를 시작할 수도 있습니다.

### 구현 세부사항:
Clojure에서 새 프로젝트를 시작하기 위해서는 Leiningen이라는 빌드 도구를 사용할 수 있습니다. 이를 통해 프로젝트의 버전, 종속성, 메인 함수 등을 설정할 수 있습니다.

## 더 알아보기:

Clojure 프로젝트를 시작하는 더 많은 정보를 원한다면 다음 링크를 참고해보세요.

### 관련 링크:
- [Clojure 문서](https://clojure.org/index)
- [Leiningen 공식 사이트](https://leiningen.org/)