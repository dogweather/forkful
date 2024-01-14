---
title:                "Clojure: 명령 줄 인수 읽기"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

커맨드 라인 인자를 읽는 것의 중요성은 여러분의 프로그램을 커스터마이징하고, 유연하게 만들며 사용자와 상호작용하기 위해서입니다.

## 사용 방법

커맨드 라인 인자를 읽는 것은 간단합니다. 먼저 ```clojure (def args (-> *command-line-args* rest)). ``` 을 사용하여 커맨드 라인 인자를 가져옵니다. 다음으로 ```clojure (println "Hello, " (first args) "!") ``` 처럼 다양한 방식으로 인자를 사용할 수 있습니다. 예를 들어, ```clojure java -jar my-program.jar John ``` 을 입력하면 "Hello, John!"이 출력됩니다.

## 심화 학습

커맨드 라인 인자를 더 깊이 이해하기 위해서는 Clojure의 ```*command-line-args*``` 함수를 더 자세히 살펴보세요. 이 함수는 Java의 ```args``` 배열로부터 커맨드 라인 인자를 가져옵니다.

## 더 알아보기

"Command Line Arguments in Clojure"는 Clojure 공식 문서에서 커맨드 라인 인자를 다루는 방법에 대해 더 자세히 설명합니다. 또한 "Clojure for the Brave and True" 책의 "Reading Command-Line Arguments" 섹션도 이 주제에 대한 유용한 정보들을 담고 있습니다. 마지막으로 "Clojure CLI"는 더 복잡한 커맨드 라인 인자를 다루는 데 유용한 도구입니다.