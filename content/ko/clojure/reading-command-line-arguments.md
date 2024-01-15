---
title:                "명령 줄 인수 읽기"
html_title:           "Clojure: 명령 줄 인수 읽기"
simple_title:         "명령 줄 인수 읽기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

커맨드 라인 인자를 읽는 법에 대해 배우고 싶은 독자들을 위한 빠르고 간단한 가이드입니다. Clojure를 사용하여 명령 줄에서 인자를 읽는 데 필요한 기본적인 지식과 예제를 제공합니다.

## 어떻게

명령 줄에서 인자를 읽는 것은 프로그램을 작성하는 데 필수적인 기능입니다. 이를 통해 사용자는 프로그램을 실행할 때 입력값을 제공할 수 있으며, 이를 통해 다양한 작업을 수행할 수 있습니다. Clojure에서는 ```clojure clojure.main```을 사용하여 명령 줄 인자를 읽을 수 있습니다.

```clojure
;; 프로그램 로직
(defn -main
  [& args]
  ;; 인자 읽기
  (println (str "인자: " args)))
```

위의 예제에서 ```-main``` 함수의 매개변수로 ```& args```를 정의하면 프로그램을 실행할 때 전달되는 명령 줄 인자를 모두 읽을 수 있습니다. 인자는 문자열의 벡터 형태로 저장되며, 이를 활용하여 프로그램 로직을 구현할 수 있습니다. 예를 들어, 다음과 같이 실행할 수 있습니다.

```sh
clj -m my-program arg1 arg2 arg3
```

위의 예제에서 ```my-program```은 Clojure로 작성된 코드를 실행하기 위한 메인 네임스페이스를 가리킵니다. 그리고 ```arg1```, ```arg2```, ```arg3```는 사용자가 프로그램으로 전달하는 입력값을 나타냅니다. 위의 예제를 실행하면 다음과 같이 출력됩니다.

```
인자: ["arg1" "arg2" "arg3"]
```

## 심화 학습

Clojure에서는 ```cli``` 라이브러리를 사용하여 조금 더 편리하게 명령 줄 인자를 읽을 수 있습니다. 이를 활용하면 입력값의 유효성 검사나 형 변환 같은 작업을 쉽게 처리할 수 있습니다. 또한, Clojure는 명령 줄 인자를 기본으로 제공하지 않지만, 외부 라이브러리를 통해 기능을 확장할 수도 있습니다.

## 관련 정보

- [Clojure 공식 홈페이지 (한국어)](https://clojure.org/guides/getting_started#_command_line_tools)
- [clojure.spec를 활용한 인자 유효성 검사 예제](https://gist.github.com/bsima/3a1b34f6772802c35fc1e53bc4403e03)
- [Clojure로 커맨드 라인 인터페이스 구현하기](https://medium.com/@meetendoshi/clojure-for-command-line-interface-c9e3db80aa5c)