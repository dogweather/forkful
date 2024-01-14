---
title:    "Clojure: 컴퓨터 프로그래밍에서 명령 줄 인수 읽기"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 왜

커맨드 라인 인수(arguments)를 읽는 것이 중요한 이유는 프로그램을 실행할 때 외부에서 입력을 받아와야 할 경우가 많기 때문입니다. 이는 사용자로부터 입력을 받아 다양한 조작을 할 수 있도록 하며, 프로그램의 유연성과 사용성을 높일 수 있습니다.

## 어떻게 하나요?

커맨드 라인 인수를 읽는 것은 간단합니다. 일반적으로 `*command-line-args*` 벡터를 사용하여 인수들을 읽어옵니다. 예를 들어, 다음과 같이 사용할 수 있습니다.

```Clojure
(defn get-args [args]
  (doseq [arg args]
    (println arg)))

(get-args *command-line-args*)
```

이 코드를 실행하면 `clojure my-program.clj arg1 arg2` 와 같이 인수들을 전달한 후 `arg1 arg2` 가 출력됩니다. 또 다른 예시로는 다음과 같은 코드가 있습니다.

```Clojure
(require '[clojure.java.io :as io])
(require '[clojure.string :as str])

(let [input-file (first *command-line-args*)]
  (when input-file
    (with-open [reader (io/reader input-file)]
      (doseq [line (line-seq reader)]
        (println (str/upper-case line))))))
```

위 코드는 전달된 입력 파일을 읽고, 각 줄을 대문자로 변환하여 출력하는 간단한 프로그램입니다.

## 딥 다이브

커맨드 라인 인수를 읽는 방법은 여러 가지가 있습니다. `clojure.tools.cli` 라이브러리를 사용하면 조금 더 복잡한 인수들을 읽고 처리할 수 있습니다. 또한, 다른 라이브러리나 샘플 코드를 참고하며 익힐 수도 있습니다. 인수들을 올바르게 처리하는 것이 중요하기 때문에 잘 알아두는 것이 좋습니다.

# 참고

- [Clojure - Command line args](https://clojure.org/guides/learn/reading_command_line_arguments)
- [Clojure - Command-line processing with tools.cli](https://clojure.org/guides/devtools/cli)
- [Introduction to Clojure - Reading command line arguments](https://clojure.org/guides/tutorials/introduction_to_command_line_utilities#_reading_command_line_arguments)